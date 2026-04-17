#!/usr/bin/env python3
"""
Push-to-talk for Speak2Text: listen for HID consumer keys (NX_SYSDEFINED), same path as skhd.

MY_GLOBE on yuanw/qmk-keymap sends consumer usage 0x029D on press and 0 on release; macOS
typically exposes the aux-control keycode as 0x9D (see SPEAK2TEXT_CONSUMER_KEYCODE).

skhd only runs actions on consumer *keydown*, so hold-to-record is implemented here and
wired via launchd. Grant Accessibility (and Microphone for sox) to this binary in
System Settings after the first switch.
"""

from __future__ import annotations

import ctypes
import os
import signal
import subprocess
import sys
import tempfile
import time

from Quartz import (
    CFDataGetBytePtr,
    CFDataGetLength,
    CFMachPortCreateRunLoopSource,
    CFRelease,
    CFRunLoopAddSource,
    CFRunLoopGetCurrent,
    CFRunLoopRun,
    CGEventCreateData,
    CGEventGetType,
    CGEventMaskBit,
    CGEventTapCreate,
    CGEventTapEnable,
    kCGEventTapOptionDefault,
    kCGHeadInsertEventTap,
    kCGSessionEventTap,
    kCFRunLoopCommonModes,
)

NX_SYSDEFINED = 14
NX_KEYDOWN = 10
NX_KEYUP = 11
NX_SUBTYPE_AUX_CONTROL_BUTTONS = 8


def _parse_sys_defined(ev) -> tuple[int, int, int] | None:
    if CGEventGetType(ev) != NX_SYSDEFINED:
        return None
    data = CGEventCreateData(None, ev)
    try:
        n = CFDataGetLength(data)
        ptr = CFDataGetBytePtr(data)
        buf = ctypes.string_at(ptr, n)
    finally:
        CFRelease(data)
    if len(buf) < 131:
        return None
    return buf[129], buf[130], buf[123]


def _notify(title: str, body: str) -> None:
    def esc(s: str) -> str:
        return s.replace("\\", "\\\\").replace('"', '\\"')

    body = body if len(body) <= 180 else body[:177] + "..."
    subprocess.run(
        [
            "osascript",
            "-e",
            f'display notification "{esc(body)}" with title "{esc(title)}"',
        ],
        check=False,
    )


def _transcribe(wav: str) -> str:
    """Transcribe a WAV file.

    If SPEAK2TEXT_SERVER_MODE is set, POST to the parakeet-mlx HTTP server
    first and fall back to CLI on failure. Otherwise use the CLI directly.
    """
    server_mode = os.environ.get("SPEAK2TEXT_SERVER_MODE")
    server_port = int(os.environ.get("SPEAK2TEXT_SERVER_PORT", "5092"))

    if server_mode:
        try:
            text = _transcribe_via_server(wav, server_port)
            if text:
                return text
        except Exception as exc:
            print(f"speak2text-ptt: server failed ({exc}), falling back to CLI", file=sys.stderr)

    # CLI fallback (or primary when server mode is off)
    bin_ = os.environ.get("SPEAK2TEXT_TRANSCRIBE_BIN")
    if not bin_:
        print("speak2text-ptt: SPEAK2TEXT_TRANSCRIBE_BIN unset", file=sys.stderr)
        return ""
    r = subprocess.run([bin_, wav], check=False, capture_output=True, text=True)
    if r.returncode != 0:
        print(r.stderr or r.stdout or "transcribe failed", file=sys.stderr)
        return ""
    return (r.stdout or "").strip()


def _transcribe_via_server(wav_path: str, port: int = 5092) -> str:
    """POST to parakeet-mlx-server and return transcript text."""
    import json
    import urllib.error
    import urllib.request

    url = f"http://127.0.0.1:{port}/v1/audio/transcriptions"
    with open(wav_path, "rb") as f:
        audio_bytes = f.read()

    boundary = b"----ParakeetBoundary7MA4YWxkTrZu0gW"
    body = (
        b"--" + boundary + b"\r\n"
        b'Content-Disposition: form-data; name="file"; filename="audio.wav"\r\n'
        b"Content-Type: audio/wav\r\n\r\n"
        + audio_bytes
        + b"\r\n"
        b"--" + boundary + b"--\r\n"
    )
    req = urllib.request.Request(
        url,
        data=body,
        method="POST",
        headers={"Content-Type": f"multipart/form-data; boundary={boundary.decode()}"},
    )
    with urllib.request.urlopen(req, timeout=30) as resp:
        result = json.loads(resp.read())
        return result.get("text", "").strip()


def _pbcopy(text: str) -> None:
    subprocess.run(["pbcopy"], input=text, text=True, check=False)


class Recorder:
    def __init__(self) -> None:
        self.proc: subprocess.Popen | None = None
        self.wav: str | None = None

    def start(self) -> None:
        if self.proc is not None:
            return
        fd, path = tempfile.mkstemp(prefix="speak2text-ptt-", suffix=".wav")
        os.close(fd)
        self.wav = path
        self.proc = subprocess.Popen(
            ["sox", "-d", "-r", "16000", "-c", "1", "-b", "16", path],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        time.sleep(0.5)
        if self.proc.poll() is not None:
            self.proc = subprocess.Popen(
                [
                    "ffmpeg",
                    "-f",
                    "avfoundation",
                    "-i",
                    ":0",
                    "-ar",
                    "16000",
                    "-ac",
                    "1",
                    "-acodec",
                    "pcm_s16le",
                    "-y",
                    "-loglevel",
                    "error",
                    path,
                ],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )

    def stop(self) -> str | None:
        if self.proc is None or self.wav is None:
            return None
        try:
            self.proc.send_signal(signal.SIGINT)
            self.proc.wait(timeout=60)
        except Exception:
            self.proc.kill()
        wav = self.wav
        self.proc = None
        self.wav = None
        if not os.path.isfile(wav) or os.path.getsize(wav) == 0:
            try:
                os.unlink(wav)
            except OSError:
                pass
            return None
        return wav


REC = Recorder()


def tap_callback(proxy, typ, event, refcon):  # noqa: ARG001
    parsed = _parse_sys_defined(event)
    if parsed is None:
        return event
    key_code, key_state, key_stype = parsed
    target = int(os.environ.get("SPEAK2TEXT_CONSUMER_KEYCODE", "157"), 0)
    if key_stype != NX_SUBTYPE_AUX_CONTROL_BUTTONS or key_code != target:
        return event

    if key_state == NX_KEYDOWN:
        try:
            REC.start()
        except FileNotFoundError as e:
            print(f"speak2text-ptt: {e}", file=sys.stderr)
            _notify("Speak2Text", "Recording failed (sox/ffmpeg missing?)")
    elif key_state == NX_KEYUP:
        wav = REC.stop()
        if not wav:
            _notify("Speak2Text", "No audio captured")
            return event
        try:
            text = _transcribe(wav)
            if not text:
                _notify("Speak2Text", "No speech detected")
            else:
                _pbcopy(text)
                _notify("Speak2Text", f"Copied: {text}")
        finally:
            try:
                os.unlink(wav)
            except OSError:
                pass

    return event


def main() -> None:
    mask = CGEventMaskBit(14)  # NX_SYSDEFINED / kCGEventSysDefined
    tap = CGEventTapCreate(
        kCGSessionEventTap,
        kCGHeadInsertEventTap,
        kCGEventTapOptionDefault,
        mask,
        tap_callback,
        None,
    )
    if not tap:
        print(
            "speak2text-ptt: CGEventTapCreate failed (Accessibility not granted?)",
            file=sys.stderr,
        )
        sys.exit(1)
    runloop_source = CFMachPortCreateRunLoopSource(None, tap, 0)
    CFRunLoopAddSource(CFRunLoopGetCurrent(), runloop_source, kCFRunLoopCommonModes)
    CGEventTapEnable(tap, True)
    CFRunLoopRun()


if __name__ == "__main__":
    main()

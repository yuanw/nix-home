# srt2txt.awk -- strip SRT frame numbering and timing, print cue text.
#
#   gawk -f srt2txt.awk [-v timestamps=1] file.srt
#
# With timestamps=1 each line is prefixed with the cue start time
# ([mm:ss], or [h:mm:ss] once past an hour).

BEGIN { stamp = ""; }

# cue index (a line holding only digits)
/^[0-9]+$/ { next }

# cue timing header: "00:01:23,456 --> 00:01:27,890"
/^[0-9][0-9]:[0-9][0-9]:[0-9][0-9],[0-9][0-9][0-9][[:space:]]*-->[[:space:]]*[0-9]/ {
    split($1, t, ":")
    gsub(/,.*/, "", t[3])
    if (t[1] + 0 > 0)
        stamp = sprintf("[%d:%02d:%02d]", t[1] + 0, t[2] + 0, t[3] + 0)
    else
        stamp = sprintf("[%02d:%02d]", t[2] + 0, t[3] + 0)
    next
}

# blank separator
/^[[:space:]]*$/ { next }

{
    line = $0
    gsub(/^[[:space:]]+/, "", line)
    gsub(/[[:space:]]+$/, "", line)
    if (line == "") next
    print (timestamps ? stamp " " line : line)
}

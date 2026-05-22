# Plan: Fix Lance for DGX Spark (sm121/GB10 Compatibility)

## Problem

The DGX Spark's GB10 GPU (compute capability **sm121**) is incompatible with flash-attention 2.x CUTLASS kernels. Flash-attn kernels compiled for sm80-sm120 crash with:

```
FATAL: kernel `fmha_cutlassF_bf16_aligned_32x128_gmem_sm80` is for sm80-sm100, but was built for sm121
```

This happens because:
1. Flash-attn 2.8.2 CUTLASS kernels only support up to sm120
2. sm121 is NOT backward compatible with sm120 binaries (new instruction set)
3. PyTorch's `scaled_dot_product_attention` with EFFICIENT_ATTENTION backend also uses these kernels

## Files to Modify (in `/Users/yuanw/workspaces/Lance`)

### 1. `modeling/lance/qwen2_navit.py`

**1a. Replace flash-attn import (line 30)**

```python
# OLD:
from flash_attn import flash_attn_varlen_func

# NEW:
from torch.nn.functional import scaled_dot_product_attention as flash_attn_varlen_func
```

But `flash_attn_varlen_func` and `scaled_dot_product_attention` have different signatures:
- `flash_attn_varlen_func(q, k, v, cu_seqlens_q, cu_seqlens_k, max_seqlen_q, max_seqlen_k)` → `(output,)` 
- `scaled_dot_product_attention(query, key, value, attn_mask=None, dropout_p=0.0, is_causal=False)` → `output`

So a direct drop-in won't work. Instead, create a wrapper function.

**Better approach: Force the `isinstance(attention_mask, List)` path**

The code already has fallback using `scaled_dot_product_attention` when `attention_mask` is a `List`. Simply change the condition to always use that path:

```python
# OLD:
if isinstance(attention_mask, List):

# NEW:
if True:
```

This appears in TWO places in `qwen2_navit.py` (the `forward` method at ~line 122 and `forward_inference` at ~line 327).

**1b. Change SDPBackend to MATH (avoid cutlass kernels)**

```python
# OLD:
with sdpa_kernel(backends=[SDPBackend.EFFICIENT_ATTENTION]):

# NEW:
with sdpa_kernel(backends=[SDPBackend.MATH]):
```

This appears in TWO places (~line 133 and ~line 338).

### 2. `modeling/qwen2/configuration_qwen2.py`

**2a. Fix `_attn_implementation` default (line 155)**

```python
# OLD:
_attn_implementation="flash_attention_2",

# NEW:
_attn_implementation="eager",
```

**2b. Set `_attn_implementation_internal` directly (after line 181)**

Add this after `self._attn_implementation = _attn_implementation`:

```python
self._attn_implementation_internal = _attn_implementation  # Force property bypass
```

This is needed because `PretrainedConfig` defines `_attn_implementation` as a property backed by `_attn_implementation_internal`. The parent's property setter doesn't work correctly when the child class doesn't call `super().__init__()`.

### 3. `modeling/qwen2/modeling_qwen2.py`

**3a. Disable flash-attn support flag (line 574)**

```python
# OLD:
_supports_flash_attn_2 = True

# NEW:
_supports_flash_attn_2 = False
```

### 4. `modeling/qwen2_5_vl/configuration_qwen2_5_vl.py`

**4a. Add `_attn_implementation` default to `Qwen2_5_VLConfig`** (if missing)

Add the same fix as 2a if the VL config also defaults to flash_attention_2.

### 5. `modeling/vit/qwen2_5_vl_vit.py`

**5a. Replace flash-attn import** (similar to step 1)

Same pattern as `qwen2_navit.py` — replace flash-attn usage or force the eager path.

### 6. `modeling/qwen2_5_vl/modeling_qwen2_5_vl.py`

**6a. Replace flash-attn import and `_supports_flash_attn_2`**

### 7. `lance_gradio_t2v_v2t.py`

**7a. Pass env var to Python (optional)**

The `GRADIO_TASK` env var is set by systemd but not read by Python. To support different tasks per port:

```python
import os
DEFAULT_TASK = os.environ.get("GRADIO_TASK", "t2v")
```

## Summary of Changes

| File | Change | Reason |
|------|--------|--------|
| `modeling/lance/qwen2_navit.py` | `if isinstance(attention_mask, List):` → `if True:` | Avoid flash-attn varlen call |
| `modeling/lance/qwen2_navit.py` | `SDPBackend.EFFICIENT_ATTENTION` → `SDPBackend.MATH` | Avoid cutlass kernel loading |
| `modeling/qwen2/configuration_qwen2.py` | `_attn_implementation="flash_attention_2"` → `"eager"` | Don't toggle flash-attn on |
| `modeling/qwen2/configuration_qwen2.py` | Add `self._attn_implementation_internal = ...` | Fix property bypass bug |
| `modeling/qwen2/modeling_qwen2.py` | `_supports_flash_attn_2 = True` → `False` | Prevent HF from enabling flash-attn |
| `modeling/qwen2_5_vl/modeling_qwen2_5_vl.py` | Same as above for VL model | Prevent flash-attn in vision encoder |
| `modeling/vit/qwen2_5_vl_vit.py` | Same as above for ViT | Prevent flash-attn in vision transformer |
| `lance_gradio_t2v_v2t.py` | Read `GRADIO_TASK` from env var | Support different tasks per port |

## Testing

1. Run `python3 inference_lance.py --task t2i --model_path downloads/Lance_3B --resolution image_768res`
2. Verify no `FATAL: kernel` errors in output
3. Verify images are generated correctly
4. Run `python3 inference_lance.py --task t2v --model_path downloads/Lance_3B_Video --resolution video_480p`
5. Verify videos are generated correctly

## Future Work

When flash-attn adds sm121 support (upstream issue #1969), revert most of these changes and use flash-attn again for better performance. The `SDPBackend.MATH` backend is slower but functional.

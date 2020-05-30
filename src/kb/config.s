.section .rodata
  .global _binary_config_kb_start
  .align 4
_binary_config_kb_start:
  .incbin "config.kb"
_binary_config_kb_end:
  .global _binary_config_kb_size
  .align 4
_binary_config_kb_size:
  .int  _binary_config_kb_end - _binary_config_kb_start

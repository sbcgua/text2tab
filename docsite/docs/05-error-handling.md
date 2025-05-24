---
sidebar_position: 5
---

# Error handling

## Error message redefinition

The exception class - `zcx_text2tab_error` - exposes `structure`, `field`, `line` and `msg` attributes (and some others). They can be used to reformat the message text if needed. For example:

```abap
  ...
  catch zcx_text2tab_error into lx. " Reformat to -> Import error at line LINE, field 'FIELD': MSG
    
    l_error_msg = 'Import error'.
    if lx->line is not initial.
      l_error_msg = |{ l_error_msg } at line { lx->line }|.
    endif.
    if lx->field is not initial.
      l_error_msg = |{ l_error_msg }, field '{ lx->field }'|.
    endif.
    l_error_msg = |{ l_error_msg }: { lx->msg }|.
    
    raise exception type lcx_my_program_error
      exporting msg = l_error_msg.
  endtry.
```

This is supported in parser only at the moment. Serializer does not produce many error on line level.

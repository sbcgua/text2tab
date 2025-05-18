---
sidebar_position: 20
---

# Utilities

## Checking version

`zif_text2tab` has the `version` attribute. And there is a helper method `check_version_fits` to check if the text2tab package has the minimal required version. To extract the current version prefer `zcl_text2tab_parser=>version( )` method.

```abap
* Assuming version is 2.1.0

" Returns false, 2.1.2 is required
zcl_text2tab_parser=>check_version_fits( 'v2.1.2' ). 

" Returns true, 2.1.0 > 2.0.0
zcl_text2tab_parser=>check_version_fits( 'v2.0.0' ). 
```

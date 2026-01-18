
# Feature Overview
- Values: Typeless constants.
  - `value A = 0x4000;`
  - `value B = A + 24;`
- Regions: Named memory regions for static and dynamic memory management.
  - `region Base[0x0000..0x2000];`
  - `region VDP_Colors[1024] @ VDP_RAM + 0x80000;`
- Records: Structured data.
  - `record CtrlRegs @ VDP_Base + 64 { a: u32, b: s8, c: u16 }`
  - `record Vec2 { x: s32, y: s32 }`
- Tables: Struct-of-Arrays style data.
  - `table Events[128] @ GameData { id: u8, type: u8, data: EventData }`
- Procedures: "Functions, subroutines, etc."

## Basic Types
- `u8`, `u16`, `u32` -- unsigned 8, 16, and 32-bit integers.
- `s8`, `s16`, `s32` -- signed 8, 16, and 32-bit integers.

if !exists('c_no_win32')
  syntax match cType "\v<U?INT(8|16|32|64)>"
  syntax match cType "\v<P?[DQ]?WORD>"
  syntax match cType "\v<P?U?CHAR>"
  syntax match cType "\v<P?U?LONG(LONG|_PTR)?>"
  syntax match cType "\v<P?U?SHORT>"
  syntax keyword cType BYTE PBYTE VOID PVOID WCHAR PWCHAR BOOLEAN PBOOLEAN BOOL SIZE_T NTSTATUS
endif

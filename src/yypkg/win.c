/*
 * yypkg - A cross-platform package manager
 * Copyright (C) 2010-2014 Adrien Nader
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef _WIN32
#define NOGDI

#include <winsock2.h>
#include <windows.h>
#include <ntdef.h>
#include <tlhelp32.h>
#include <stdio.h>

#endif

#define CAML_NAME_SPACE

#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#ifndef _WIN32

value
yy_remove(value s)
{
  CAMLparam1(s);
  caml_failwith("yy_remove() is only implemented on Windows.");
  CAMLnoreturn;
}

value create_reparse_point(value s)
{
  CAMLparam1(s);
  caml_failwith("create_reparse_point() is only implemented on Windows.");
  CAMLnoreturn;
}

#else

static void Noreturn
custom_failwith(int e, const char *prefix)
{
  char win_msg[512] = { 0 };
  char exn_msg[512] = { 0 };
  BOOL ret;

  ret = FormatMessageA(
      FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      e,
      0,
      win_msg,
      sizeof(win_msg),
      NULL
  );

  snprintf(exn_msg, sizeof(exn_msg), "%s%s",
      ((prefix != NULL) ? prefix : ""), 
      ((ret == 0) ?  "Couldn't get the error message." : win_msg));

  caml_failwith(exn_msg);
}

static void
remove_reparse_point(const char *path)
{
  BOOL ret;
  HANDLE h;
  DWORD l;
  REPARSE_DATA_BUFFER rpd = { 0 };

  h = CreateFile(
      path,
      GENERIC_READ | GENERIC_WRITE,
      0,
      NULL,
      OPEN_EXISTING,
      FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS,
      NULL
  );

  if (h == INVALID_HANDLE_VALUE) {
    custom_failwith(GetLastError(), "CreateFile, ");
  }

  rpd.ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;

  ret = DeviceIoControl(
      h,
      FSCTL_DELETE_REPARSE_POINT,
      &rpd,
      REPARSE_DATA_BUFFER_HEADER_SIZE, /* ? */
      NULL,
      0,
      &l,
      NULL
  );

  CloseHandle(h);

  if (ret == 0) {
    custom_failwith(GetLastError(),
        "DeviceIoControl(FSCTL_DELETE_REPARSE_POINT), ");
  }
}

value
yy_remove(value ml_path)
{
  CAMLparam1(ml_path);
  const char *path;
  DWORD file_attributes;

  path = String_val(ml_path);

  file_attributes = GetFileAttributes(path);

  if (file_attributes == INVALID_FILE_ATTRIBUTES) {
    CAMLreturn(Val_unit);
  }

  if ((file_attributes & FILE_ATTRIBUTE_DIRECTORY) == 0) {
    if (unlink(String_val(ml_path)) == -1) {
      uerror("unlink", ml_path);
    }
    CAMLreturn(Val_unit);
  }

  if ((file_attributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0) {
    remove_reparse_point(path);
  }

  if (RemoveDirectory(path) == 0) {
    custom_failwith(GetLastError(),
        "Trying to remove reparse point directory, ");
  }

  CAMLreturn(Val_unit);
}

static void *
get_proc(HMODULE h, const char *name)
{
  void *f;
  char buf[128];

  f = GetProcAddress(h, name);
  
  if (f == NULL) {
    snprintf(buf, sizeof(buf), "GetProcAddress, %s: ");
    custom_failwith(GetLastError(), buf);
  }
  else {
    return f;
  }
}

static HANDLE
get_empty_directory_handle(const char *path)
{
  DWORD file_attributes;
  char buf[PATH_MAX+64];
  HANDLE h;

  file_attributes = GetFileAttributes(path);

  if (file_attributes == INVALID_FILE_ATTRIBUTES) {
    if (CreateDirectory(path, NULL) == 0) {
      custom_failwith(GetLastError(),
          "Trying to create reparse point directory, ");
    }
  }
  else {
    if ((file_attributes & FILE_ATTRIBUTE_DIRECTORY) == 0) {
      snprintf(buf, sizeof(buf), "`%s' exists and isn't a directory.", path);
      caml_failwith(buf);
    }
#if 0
    else {
      if 
    }
#endif
  }

  h = CreateFile(
      path,
      GENERIC_WRITE,
      0,
      NULL,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS,
      NULL
  );

  if (h == INVALID_HANDLE_VALUE) {
    custom_failwith(GetLastError(), "CreateFile, ");
  }

  return h;
}

value
create_reparse_point(value ml_to, value ml_at)
{
  CAMLparam2(ml_at, ml_to);
  char *atA, *toA;
  HANDLE h;
  REPARSE_DATA_BUFFER *rpd;
  BOOL ret;
  int e;
  TCHAR toA_full[MAX_PATH] = { 0 };
	WCHAR toW[MAX_PATH] = { 0 };
	UNICODE_STRING TargetNTPath = { 0 };
  HMODULE h_ntdll;
  BOOL (WINAPI *RtlDosPathNameToNtPathName_U)(PCWSTR, UNICODE_STRING *, PCWSTR *, VOID *);
  VOID (WINAPI *RtlFreeUnicodeString)(PUNICODE_STRING);
  size_t print_name_offset, print_name_length, reparse_data_buffer_size;
  DWORD data_size;

  atA = String_val(ml_at);
  toA = String_val(ml_to);

  h = get_empty_directory_handle(atA);

  h_ntdll = GetModuleHandle("NTDLL");

  RtlDosPathNameToNtPathName_U = get_proc(h_ntdll, "RtlDosPathNameToNtPathName_U");
  RtlFreeUnicodeString = get_proc(h_ntdll, "RtlFreeUnicodeString");

  CloseHandle(h_ntdll);

  if (GetFullPathName(toA, MAX_PATH, toA_full, NULL) == 0) {
    custom_failwith(GetLastError(), "GetFullPathName, ");
  }

  if (MultiByteToWideChar(CP_UTF8, 0, toA_full, -1, toW, MAX_PATH) == 0) {
    custom_failwith(GetLastError(), "MultiByteToWideChar, ");
  }

  if (RtlDosPathNameToNtPathName_U(toW, &TargetNTPath, NULL, NULL) == 0) {
    custom_failwith(GetLastError(), "RtlDosPathNameToNtPathName_U, ");
  }

  /* Allocate a buffer large enough to hold both strings, including trailing NULs */
  print_name_offset = TargetNTPath.Length + sizeof(WCHAR);
  print_name_length = wcslen(toW) * sizeof(WCHAR);
  reparse_data_buffer_size = print_name_offset + print_name_length + sizeof(WCHAR);

  data_size = FIELD_OFFSET(REPARSE_DATA_BUFFER, MountPointReparseBuffer.PathBuffer) + reparse_data_buffer_size;

  rpd = malloc(data_size);

  /* Fill it out and use it to turn the directory into a reparse point */
  rpd->ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;

  rpd->ReparseDataLength = data_size - FIELD_OFFSET(REPARSE_DATA_BUFFER, MountPointReparseBuffer);

  rpd->Reserved = 0;

#define MPRB (rpd->MountPointReparseBuffer)

  MPRB.SubstituteNameOffset = 0;
  MPRB.SubstituteNameLength = TargetNTPath.Length;

  MPRB.PrintNameOffset = print_name_offset;
  MPRB.PrintNameLength = print_name_length;

  memcpy(MPRB.PathBuffer, TargetNTPath.Buffer, TargetNTPath.Length);
  ((char*)MPRB.PathBuffer)[MPRB.SubstituteNameLength] = 0;

  wcscpy((WCHAR *)(((char *)MPRB.PathBuffer) + MPRB.PrintNameOffset),
      toW);

#undef MPRB

  ret = DeviceIoControl(
      h,
      FSCTL_SET_REPARSE_POINT,
      rpd,
      data_size,
      NULL,
      0,
      &data_size,
      NULL
  );

  e = GetLastError();

  CloseHandle(h);
  RtlFreeUnicodeString(&TargetNTPath);

  if (ret == 0) {
    custom_failwith(e, "DeviceIoControl(FSCTL_SET_REPARSE_POINT), ");
  }

  CAMLreturn(Val_unit);
}

static char *
get_parent_process_name(void)
{
  HANDLE snap;
  PROCESSENTRY32 pe32;
  char found = 0;
  char *name = NULL;
  DWORD parent_id;

  snap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (snap == INVALID_HANDLE_VALUE) {
    return NULL;
  }

  pe32.dwSize = sizeof(pe32);

  if (!Process32First(snap, &pe32)) {
    CloseHandle(snap);
    return NULL;
  }

  do {
    if (pe32.th32ProcessID == GetCurrentProcessId()) {
      found = 1;
      parent_id = pe32.th32ParentProcessID;
      break;
    }
  } while (Process32Next(snap, &pe32));

  if (!found) {
    CloseHandle(snap);
    return NULL;
  }

  if (!Process32First(snap, &pe32)) {
    CloseHandle(snap);
    return NULL;
  }

  do {
    if (pe32.th32ProcessID == parent_id) {
      name = strdup(pe32.szExeFile);
      break;
    }
  } while (Process32Next(snap, &pe32));

  CloseHandle(snap);
  return name;
}

#endif /* _WIN32 */

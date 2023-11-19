#pragma once

#include <exception>
#include <filesystem> // Project/Properties/Language/C++ Language Standard => ISO C++17 Standard (/std:c++17)
#include <iostream>
#include <stdio.h>

namespace fs = std::filesystem; // Project/Properties/Language/C++ Language Standard => ISO C++17 Standard (/std:c++17).

using namespace std;

#ifdef DLLDIR_EX
#define DLLDIR  __declspec(dllexport)   // export DLL information
#else
#define DLLDIR  __declspec(dllimport)   // import DLL information
#endif 

extern "C" DLLDIR void CopyDirContent32(wchar_t* sourceString, wchar_t* targetParentString, int c, int o);
//extern "C" DLLDIR void CopyDirContent64(wchar_t* sourceString, wchar_t* targetParentString, int c, int o);


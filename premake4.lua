solution "Assembler"
	location ("build/")
	configurations {"Debug", "Release"}

	project "Assembler"
		location "build/assembler"
		kind "ConsoleApp"
		language "c"
		files 
		{
			"./**.c", 
			"./**.h"
		}
		objdir "obj"
		includedirs {"include"}

		configuration "Debug"
			defines "_DEBUG"
			flags "Symbols"
			targetdir "bin/debug"
		
		configuration "Release"
			defines "NDEBUG"
			targetdir "bin/release"
			flags {"OptimizeSpeed", "NoFramePointer", "ExtraWarnings", "NoEditAndContinue"}
package Global_Version is

Major_Version : constant String := "2003";
Minor_Version : constant String := "24";

--pragma Linker_Options ("-Wl,--major-image-version=" & Major_Version);
--pragma Linker_Options ("-Wl,--minor-image-version=" & Minor_Version);

end Global_Version;

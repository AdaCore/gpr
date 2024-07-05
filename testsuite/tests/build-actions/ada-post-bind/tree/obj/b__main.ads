pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Pro 25.0w (20240618-133)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_main" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#50b513e3#;
   pragma Export (C, u00001, "mainB");
   u00002 : constant Version_32 := 16#7606b412#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0626cc96#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#b106b4d6#;
   pragma Export (C, u00004, "pkgB");
   u00005 : constant Version_32 := 16#3fa39442#;
   pragma Export (C, u00005, "pkgS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#a201b8c5#;
   pragma Export (C, u00007, "ada__strings__text_buffersB");
   u00008 : constant Version_32 := 16#a7cfd09b#;
   pragma Export (C, u00008, "ada__strings__text_buffersS");
   u00009 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00009, "ada__stringsS");
   u00010 : constant Version_32 := 16#14286b0f#;
   pragma Export (C, u00010, "systemS");
   u00011 : constant Version_32 := 16#8128890d#;
   pragma Export (C, u00011, "system__exception_tableB");
   u00012 : constant Version_32 := 16#99031d16#;
   pragma Export (C, u00012, "system__exception_tableS");
   u00013 : constant Version_32 := 16#bb69bacb#;
   pragma Export (C, u00013, "system__soft_linksB");
   u00014 : constant Version_32 := 16#036ac175#;
   pragma Export (C, u00014, "system__soft_linksS");
   u00015 : constant Version_32 := 16#14799883#;
   pragma Export (C, u00015, "system__secondary_stackB");
   u00016 : constant Version_32 := 16#bae33a03#;
   pragma Export (C, u00016, "system__secondary_stackS");
   u00017 : constant Version_32 := 16#dc6bfe14#;
   pragma Export (C, u00017, "ada__exceptionsB");
   u00018 : constant Version_32 := 16#64d9391c#;
   pragma Export (C, u00018, "ada__exceptionsS");
   u00019 : constant Version_32 := 16#41763aa4#;
   pragma Export (C, u00019, "ada__exceptions__last_chance_handlerB");
   u00020 : constant Version_32 := 16#a028f72d#;
   pragma Export (C, u00020, "ada__exceptions__last_chance_handlerS");
   u00021 : constant Version_32 := 16#268dd43d#;
   pragma Export (C, u00021, "system__exceptionsS");
   u00022 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00022, "system__exceptions__machineB");
   u00023 : constant Version_32 := 16#46355a4a#;
   pragma Export (C, u00023, "system__exceptions__machineS");
   u00024 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00024, "system__exceptions_debugB");
   u00025 : constant Version_32 := 16#2426335c#;
   pragma Export (C, u00025, "system__exceptions_debugS");
   u00026 : constant Version_32 := 16#36b7284e#;
   pragma Export (C, u00026, "system__img_intS");
   u00027 : constant Version_32 := 16#f2c63a02#;
   pragma Export (C, u00027, "ada__numericsS");
   u00028 : constant Version_32 := 16#174f5472#;
   pragma Export (C, u00028, "ada__numerics__big_numbersS");
   u00029 : constant Version_32 := 16#ee021456#;
   pragma Export (C, u00029, "system__unsigned_typesS");
   u00030 : constant Version_32 := 16#d8f6bfe7#;
   pragma Export (C, u00030, "system__storage_elementsS");
   u00031 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00031, "system__tracebackB");
   u00032 : constant Version_32 := 16#92b29fb2#;
   pragma Export (C, u00032, "system__tracebackS");
   u00033 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00033, "system__traceback_entriesB");
   u00034 : constant Version_32 := 16#dc34d483#;
   pragma Export (C, u00034, "system__traceback_entriesS");
   u00035 : constant Version_32 := 16#f44a6fee#;
   pragma Export (C, u00035, "system__traceback__symbolicB");
   u00036 : constant Version_32 := 16#140ceb78#;
   pragma Export (C, u00036, "system__traceback__symbolicS");
   u00037 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00037, "ada__containersS");
   u00038 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00038, "ada__exceptions__tracebackB");
   u00039 : constant Version_32 := 16#26ed0985#;
   pragma Export (C, u00039, "ada__exceptions__tracebackS");
   u00040 : constant Version_32 := 16#9111f9c1#;
   pragma Export (C, u00040, "interfacesS");
   u00041 : constant Version_32 := 16#401f6fd6#;
   pragma Export (C, u00041, "interfaces__cB");
   u00042 : constant Version_32 := 16#59e2f8b5#;
   pragma Export (C, u00042, "interfaces__cS");
   u00043 : constant Version_32 := 16#a43efea2#;
   pragma Export (C, u00043, "system__parametersB");
   u00044 : constant Version_32 := 16#21bf971e#;
   pragma Export (C, u00044, "system__parametersS");
   u00045 : constant Version_32 := 16#0978786d#;
   pragma Export (C, u00045, "system__bounded_stringsB");
   u00046 : constant Version_32 := 16#63d54a16#;
   pragma Export (C, u00046, "system__bounded_stringsS");
   u00047 : constant Version_32 := 16#9f0c0c80#;
   pragma Export (C, u00047, "system__crtlS");
   u00048 : constant Version_32 := 16#a604bd9c#;
   pragma Export (C, u00048, "system__dwarf_linesB");
   u00049 : constant Version_32 := 16#f38e5e19#;
   pragma Export (C, u00049, "system__dwarf_linesS");
   u00050 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00050, "ada__charactersS");
   u00051 : constant Version_32 := 16#9de61c25#;
   pragma Export (C, u00051, "ada__characters__handlingB");
   u00052 : constant Version_32 := 16#729cc5db#;
   pragma Export (C, u00052, "ada__characters__handlingS");
   u00053 : constant Version_32 := 16#cde9ea2d#;
   pragma Export (C, u00053, "ada__characters__latin_1S");
   u00054 : constant Version_32 := 16#c5e1e773#;
   pragma Export (C, u00054, "ada__strings__mapsB");
   u00055 : constant Version_32 := 16#6feaa257#;
   pragma Export (C, u00055, "ada__strings__mapsS");
   u00056 : constant Version_32 := 16#b451a498#;
   pragma Export (C, u00056, "system__bit_opsB");
   u00057 : constant Version_32 := 16#d9dbc733#;
   pragma Export (C, u00057, "system__bit_opsS");
   u00058 : constant Version_32 := 16#b459efcb#;
   pragma Export (C, u00058, "ada__strings__maps__constantsS");
   u00059 : constant Version_32 := 16#f9910acc#;
   pragma Export (C, u00059, "system__address_imageB");
   u00060 : constant Version_32 := 16#b5c4f635#;
   pragma Export (C, u00060, "system__address_imageS");
   u00061 : constant Version_32 := 16#219681aa#;
   pragma Export (C, u00061, "system__img_address_32S");
   u00062 : constant Version_32 := 16#0cb62028#;
   pragma Export (C, u00062, "system__img_address_64S");
   u00063 : constant Version_32 := 16#7da15eb1#;
   pragma Export (C, u00063, "system__img_unsS");
   u00064 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00064, "system__ioB");
   u00065 : constant Version_32 := 16#8a6a9c40#;
   pragma Export (C, u00065, "system__ioS");
   u00066 : constant Version_32 := 16#e15ca368#;
   pragma Export (C, u00066, "system__mmapB");
   u00067 : constant Version_32 := 16#99159588#;
   pragma Export (C, u00067, "system__mmapS");
   u00068 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#dd82c35a#;
   pragma Export (C, u00069, "system__mmap__os_interfaceB");
   u00070 : constant Version_32 := 16#37fd3b64#;
   pragma Export (C, u00070, "system__mmap__os_interfaceS");
   u00071 : constant Version_32 := 16#8b2fdabc#;
   pragma Export (C, u00071, "system__mmap__unixS");
   u00072 : constant Version_32 := 16#6cba8c27#;
   pragma Export (C, u00072, "system__os_libB");
   u00073 : constant Version_32 := 16#ee44bb50#;
   pragma Export (C, u00073, "system__os_libS");
   u00074 : constant Version_32 := 16#94d23d25#;
   pragma Export (C, u00074, "system__atomic_operations__test_and_setB");
   u00075 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00075, "system__atomic_operations__test_and_setS");
   u00076 : constant Version_32 := 16#d34b112a#;
   pragma Export (C, u00076, "system__atomic_operationsS");
   u00077 : constant Version_32 := 16#553a519e#;
   pragma Export (C, u00077, "system__atomic_primitivesB");
   u00078 : constant Version_32 := 16#1cf8e0ec#;
   pragma Export (C, u00078, "system__atomic_primitivesS");
   u00079 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00079, "system__case_utilB");
   u00080 : constant Version_32 := 16#db3bbc5a#;
   pragma Export (C, u00080, "system__case_utilS");
   u00081 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00081, "system__stringsB");
   u00082 : constant Version_32 := 16#8faa6b17#;
   pragma Export (C, u00082, "system__stringsS");
   u00083 : constant Version_32 := 16#ae783715#;
   pragma Export (C, u00083, "system__object_readerB");
   u00084 : constant Version_32 := 16#87571f07#;
   pragma Export (C, u00084, "system__object_readerS");
   u00085 : constant Version_32 := 16#75406883#;
   pragma Export (C, u00085, "system__val_lliS");
   u00086 : constant Version_32 := 16#838eea00#;
   pragma Export (C, u00086, "system__val_lluS");
   u00087 : constant Version_32 := 16#47d9a892#;
   pragma Export (C, u00087, "system__sparkS");
   u00088 : constant Version_32 := 16#a571a4dc#;
   pragma Export (C, u00088, "system__spark__cut_operationsB");
   u00089 : constant Version_32 := 16#629c0fb7#;
   pragma Export (C, u00089, "system__spark__cut_operationsS");
   u00090 : constant Version_32 := 16#1bac5121#;
   pragma Export (C, u00090, "system__val_utilB");
   u00091 : constant Version_32 := 16#f0bdcc7b#;
   pragma Export (C, u00091, "system__val_utilS");
   u00092 : constant Version_32 := 16#fce7eeb4#;
   pragma Export (C, u00092, "system__exception_tracesB");
   u00093 : constant Version_32 := 16#f8b00269#;
   pragma Export (C, u00093, "system__exception_tracesS");
   u00094 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00094, "system__wch_conB");
   u00095 : constant Version_32 := 16#cd2b486c#;
   pragma Export (C, u00095, "system__wch_conS");
   u00096 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00096, "system__wch_stwB");
   u00097 : constant Version_32 := 16#e03a646d#;
   pragma Export (C, u00097, "system__wch_stwS");
   u00098 : constant Version_32 := 16#7cd63de5#;
   pragma Export (C, u00098, "system__wch_cnvB");
   u00099 : constant Version_32 := 16#cbeb821c#;
   pragma Export (C, u00099, "system__wch_cnvS");
   u00100 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00100, "system__wch_jisB");
   u00101 : constant Version_32 := 16#7e5ce036#;
   pragma Export (C, u00101, "system__wch_jisS");
   u00102 : constant Version_32 := 16#0286ce9f#;
   pragma Export (C, u00102, "system__soft_links__initializeB");
   u00103 : constant Version_32 := 16#68e79400#;
   pragma Export (C, u00103, "system__soft_links__initializeS");
   u00104 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00104, "system__stack_checkingB");
   u00105 : constant Version_32 := 16#d3777e19#;
   pragma Export (C, u00105, "system__stack_checkingS");
   u00106 : constant Version_32 := 16#8b7604c4#;
   pragma Export (C, u00106, "ada__strings__utf_encodingB");
   u00107 : constant Version_32 := 16#c9e86997#;
   pragma Export (C, u00107, "ada__strings__utf_encodingS");
   u00108 : constant Version_32 := 16#bb780f45#;
   pragma Export (C, u00108, "ada__strings__utf_encoding__stringsB");
   u00109 : constant Version_32 := 16#b85ff4b6#;
   pragma Export (C, u00109, "ada__strings__utf_encoding__stringsS");
   u00110 : constant Version_32 := 16#d1d1ed0b#;
   pragma Export (C, u00110, "ada__strings__utf_encoding__wide_stringsB");
   u00111 : constant Version_32 := 16#5678478f#;
   pragma Export (C, u00111, "ada__strings__utf_encoding__wide_stringsS");
   u00112 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00112, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00113 : constant Version_32 := 16#d7af3358#;
   pragma Export (C, u00113, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00114 : constant Version_32 := 16#683e3bb7#;
   pragma Export (C, u00114, "ada__tagsB");
   u00115 : constant Version_32 := 16#4ff764f3#;
   pragma Export (C, u00115, "ada__tagsS");
   u00116 : constant Version_32 := 16#3548d972#;
   pragma Export (C, u00116, "system__htableB");
   u00117 : constant Version_32 := 16#95f133e4#;
   pragma Export (C, u00117, "system__htableS");
   u00118 : constant Version_32 := 16#1f1abe38#;
   pragma Export (C, u00118, "system__string_hashB");
   u00119 : constant Version_32 := 16#32b4b39b#;
   pragma Export (C, u00119, "system__string_hashS");
   u00120 : constant Version_32 := 16#27ac21ac#;
   pragma Export (C, u00120, "ada__text_ioB");
   u00121 : constant Version_32 := 16#04ab031f#;
   pragma Export (C, u00121, "ada__text_ioS");
   u00122 : constant Version_32 := 16#b228eb1e#;
   pragma Export (C, u00122, "ada__streamsB");
   u00123 : constant Version_32 := 16#613fe11c#;
   pragma Export (C, u00123, "ada__streamsS");
   u00124 : constant Version_32 := 16#05222263#;
   pragma Export (C, u00124, "system__put_imagesB");
   u00125 : constant Version_32 := 16#08866c10#;
   pragma Export (C, u00125, "system__put_imagesS");
   u00126 : constant Version_32 := 16#22b9eb9f#;
   pragma Export (C, u00126, "ada__strings__text_buffers__utilsB");
   u00127 : constant Version_32 := 16#89062ac3#;
   pragma Export (C, u00127, "ada__strings__text_buffers__utilsS");
   u00128 : constant Version_32 := 16#1cacf006#;
   pragma Export (C, u00128, "interfaces__c_streamsB");
   u00129 : constant Version_32 := 16#d07279c2#;
   pragma Export (C, u00129, "interfaces__c_streamsS");
   u00130 : constant Version_32 := 16#b1794e9b#;
   pragma Export (C, u00130, "system__file_ioB");
   u00131 : constant Version_32 := 16#72673e49#;
   pragma Export (C, u00131, "system__file_ioS");
   u00132 : constant Version_32 := 16#c34b231e#;
   pragma Export (C, u00132, "ada__finalizationS");
   u00133 : constant Version_32 := 16#d00f339c#;
   pragma Export (C, u00133, "system__finalization_rootB");
   u00134 : constant Version_32 := 16#1e5455db#;
   pragma Export (C, u00134, "system__finalization_rootS");
   u00135 : constant Version_32 := 16#3099be46#;
   pragma Export (C, u00135, "system__finalization_primitivesB");
   u00136 : constant Version_32 := 16#cb7e49f3#;
   pragma Export (C, u00136, "system__finalization_primitivesS");
   u00137 : constant Version_32 := 16#42048880#;
   pragma Export (C, u00137, "system__os_locksS");
   u00138 : constant Version_32 := 16#d7185d6e#;
   pragma Export (C, u00138, "system__os_constantsS");
   u00139 : constant Version_32 := 16#9e5df665#;
   pragma Export (C, u00139, "system__file_control_blockS");
   u00140 : constant Version_32 := 16#b320deb0#;
   pragma Export (C, u00140, "dep_twoB");
   u00141 : constant Version_32 := 16#05c87a67#;
   pragma Export (C, u00141, "dep_twoS");
   u00142 : constant Version_32 := 16#c912c64c#;
   pragma Export (C, u00142, "system__memoryB");
   u00143 : constant Version_32 := 16#0cbcf715#;
   pragma Export (C, u00143, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.atomic_operations%s
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.spark%s
   --  system.spark.cut_operations%s
   --  system.spark.cut_operations%b
   --  system.storage_elements%s
   --  system.img_address_32%s
   --  system.img_address_64%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.atomic_operations.test_and_set%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.big_numbers%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.exceptions%s
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  ada.characters.handling%b
   --  system.atomic_operations.test_and_set%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.img_int%s
   --  system.img_uns%s
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap%b
   --  system.mmap.unix%s
   --  system.mmap.os_interface%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.strings%s
   --  ada.strings.utf_encoding.strings%b
   --  ada.strings.utf_encoding.wide_strings%s
   --  ada.strings.utf_encoding.wide_strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  system.os_constants%s
   --  system.os_locks%s
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.finalization_primitives%s
   --  system.finalization_primitives%b
   --  system.file_io%s
   --  system.file_io%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  dep_two%s
   --  dep_two%b
   --  pkg%s
   --  pkg%b
   --  main%b
   --  END ELABORATION ORDER

end ada_main;

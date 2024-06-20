pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E073 : Short_Integer; pragma Import (Ada, E073, "system__os_lib_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__exceptions_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__exception_table_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "ada__containers_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "ada__numerics_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings__maps_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__strings__maps__constants_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "interfaces__c_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "system__object_reader_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "system__dwarf_lines_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "system__soft_links__initialize_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "system__traceback__symbolic_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "ada__assertions_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__strings__utf_encoding_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "ada__tags_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__strings__text_buffers_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "gnat_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "interfaces__c__strings_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "ada__streams_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "system__file_control_block_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "system__finalization_root_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "ada__finalization_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "system__finalization_primitives_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "system__file_io_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "ada__streams__stream_io_E");
   E185 : Short_Integer; pragma Import (Ada, E185, "system__storage_pools_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "system__storage_pools__subpools_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "ada__strings__unbounded_E");
   E622 : Short_Integer; pragma Import (Ada, E622, "ada__strings__wide_wide_maps_E");
   E639 : Short_Integer; pragma Import (Ada, E639, "ada__strings__wide_wide_unbounded_E");
   E292 : Short_Integer; pragma Import (Ada, E292, "system__task_info_E");
   E241 : Short_Integer; pragma Import (Ada, E241, "system__regpat_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "ada__calendar_E");
   E673 : Short_Integer; pragma Import (Ada, E673, "ada__calendar__delays_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "ada__calendar__time_zones_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "ada__text_io_E");
   E502 : Short_Integer; pragma Import (Ada, E502, "ada__text_io__text_streams_E");
   E671 : Short_Integer; pragma Import (Ada, E671, "gnat__byte_order_mark_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "gnat__calendar_E");
   E377 : Short_Integer; pragma Import (Ada, E377, "gnat__directory_operations_E");
   E567 : Short_Integer; pragma Import (Ada, E567, "gnat__dynamic_htables_E");
   E361 : Short_Integer; pragma Import (Ada, E361, "gnat__secure_hashes_E");
   E363 : Short_Integer; pragma Import (Ada, E363, "gnat__secure_hashes__md5_E");
   E359 : Short_Integer; pragma Import (Ada, E359, "gnat__md5_E");
   E835 : Short_Integer; pragma Import (Ada, E835, "gnat__secure_hashes__sha1_E");
   E841 : Short_Integer; pragma Import (Ada, E841, "gnat__secure_hashes__sha2_common_E");
   E839 : Short_Integer; pragma Import (Ada, E839, "gnat__secure_hashes__sha2_32_E");
   E833 : Short_Integer; pragma Import (Ada, E833, "gnat__sha1_E");
   E837 : Short_Integer; pragma Import (Ada, E837, "gnat__sha256_E");
   E400 : Short_Integer; pragma Import (Ada, E400, "gnat__string_split_E");
   E446 : Short_Integer; pragma Import (Ada, E446, "system__checked_pools_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "system__task_primitives__operations_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "gnat__calendar__time_io_E");
   E441 : Short_Integer; pragma Import (Ada, E441, "gnat__debug_pools_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "system__pool_global_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "gnat__expect_E");
   E300 : Short_Integer; pragma Import (Ada, E300, "gnat__formatted_string_E");
   E392 : Short_Integer; pragma Import (Ada, E392, "system__random_seed_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "system__regexp_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "ada__directories_E");
   E785 : Short_Integer; pragma Import (Ada, E785, "system__tasking__protected_objects_E");
   E698 : Short_Integer; pragma Import (Ada, E698, "gnatcoll__gmp__integers_E");
   E666 : Short_Integer; pragma Import (Ada, E666, "gpr_parser_support__errors_E");
   E469 : Short_Integer; pragma Import (Ada, E469, "unicode_E");
   E271 : Short_Integer; pragma Import (Ada, E271, "gnatcoll__atomic_E");
   E595 : Short_Integer; pragma Import (Ada, E595, "gnatcoll__directed_graph_E");
   E439 : Short_Integer; pragma Import (Ada, E439, "gnatcoll__memory_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "gnatcoll__os_E");
   E277 : Short_Integer; pragma Import (Ada, E277, "gnatcoll__storage_pools__headers_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "gnatcoll__refcount_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "gnatcoll__string_builders_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "gnatcoll__os__fs_E");
   E844 : Short_Integer; pragma Import (Ada, E844, "gnatcoll__hash__blake3_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "gnatcoll__os__stat_E");
   E794 : Short_Integer; pragma Import (Ada, E794, "gnatcoll__os__dir_E");
   E424 : Short_Integer; pragma Import (Ada, E424, "gnatcoll__string_list_builders_E");
   E422 : Short_Integer; pragma Import (Ada, E422, "gnatcoll__os__process_types_E");
   E416 : Short_Integer; pragma Import (Ada, E416, "gnatcoll__os__process_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "gnatcoll__strings_impl_E");
   E830 : Short_Integer; pragma Import (Ada, E830, "gnatcoll__os__fsutil_E");
   E828 : Short_Integer; pragma Import (Ada, E828, "gnatcoll__file_indexes_E");
   E269 : Short_Integer; pragma Import (Ada, E269, "gnatcoll__strings_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "gnatcoll__strings_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "gnatcoll__mmap_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "gnatcoll__mmap__system_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "gnatcoll__buffer_E");
   E848 : Short_Integer; pragma Import (Ada, E848, "gnatcoll__json_E");
   E850 : Short_Integer; pragma Import (Ada, E850, "gnatcoll__json__utility_E");
   E448 : Short_Integer; pragma Import (Ada, E448, "gnatcoll__templates_E");
   E450 : Short_Integer; pragma Import (Ada, E450, "gnatcoll__terminal_E");
   E410 : Short_Integer; pragma Import (Ada, E410, "gnatcoll__tribooleans_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "gnatcoll__utils_E");
   E372 : Short_Integer; pragma Import (Ada, E372, "gnatcoll__io_E");
   E381 : Short_Integer; pragma Import (Ada, E381, "gnatcoll__path_E");
   E379 : Short_Integer; pragma Import (Ada, E379, "gnatcoll__io__native_E");
   E384 : Short_Integer; pragma Import (Ada, E384, "gnatcoll__remote_E");
   E388 : Short_Integer; pragma Import (Ada, E388, "gnatcoll__remote__db_E");
   E375 : Short_Integer; pragma Import (Ada, E375, "gnatcoll__io__remote_E");
   E383 : Short_Integer; pragma Import (Ada, E383, "gnatcoll__io__remote__unix_E");
   E386 : Short_Integer; pragma Import (Ada, E386, "gnatcoll__io__remote__windows_E");
   E368 : Short_Integer; pragma Import (Ada, E368, "gnatcoll__vfs_E");
   E435 : Short_Integer; pragma Import (Ada, E435, "gnatcoll__traces_E");
   E635 : Short_Integer; pragma Import (Ada, E635, "gnatcoll__iconv_E");
   E454 : Short_Integer; pragma Import (Ada, E454, "gnatcoll__vfs_utils_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "gpr2_E");
   E414 : Short_Integer; pragma Import (Ada, E414, "gpr2__environment_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "gpr2__path_name_E");
   E426 : Short_Integer; pragma Import (Ada, E426, "gpr2__path_name__set_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "gpr2__source_reference_E");
   E298 : Short_Integer; pragma Import (Ada, E298, "gpr2__message_E");
   E396 : Short_Integer; pragma Import (Ada, E396, "gpr2__message__reporter_E");
   E790 : Short_Integer; pragma Import (Ada, E790, "gpr2__source_reference__attribute_E");
   E780 : Short_Integer; pragma Import (Ada, E780, "gpr2__source_reference__pack_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "gpr2__source_reference__text_value_E");
   E776 : Short_Integer; pragma Import (Ada, E776, "gpr2__source_reference__identifier_E");
   E402 : Short_Integer; pragma Import (Ada, E402, "gpr2__source_reference__value_E");
   E398 : Short_Integer; pragma Import (Ada, E398, "gpr2__containers_E");
   E600 : Short_Integer; pragma Import (Ada, E600, "gpr2__build__unit_info_E");
   E602 : Short_Integer; pragma Import (Ada, E602, "gpr2__build__unit_info__list_E");
   E431 : Short_Integer; pragma Import (Ada, E431, "gpr2__context_E");
   E769 : Short_Integer; pragma Import (Ada, E769, "gpr2__builtin_E");
   E394 : Short_Integer; pragma Import (Ada, E394, "gpr2__log_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "gpr2__build__ali_parser_E");
   E408 : Short_Integer; pragma Import (Ada, E408, "gpr2__project_E");
   E756 : Short_Integer; pragma Import (Ada, E756, "gpr2__project__attribute_index_E");
   E763 : Short_Integer; pragma Import (Ada, E763, "gpr2__project__import_E");
   E765 : Short_Integer; pragma Import (Ada, E765, "gpr2__project__import__set_E");
   E587 : Short_Integer; pragma Import (Ada, E587, "gpr2__project__registry__pack_E");
   E859 : Short_Integer; pragma Import (Ada, E859, "gpr2__project__registry__pack__description_E");
   E826 : Short_Integer; pragma Import (Ada, E826, "gpr2__utils__hash_E");
   E823 : Short_Integer; pragma Import (Ada, E823, "gpr2__build__artifacts_E");
   E803 : Short_Integer; pragma Import (Ada, E803, "gpr2__view_base_internal_E");
   E805 : Short_Integer; pragma Import (Ada, E805, "gpr2__view_ids_E");
   E807 : Short_Integer; pragma Import (Ada, E807, "gpr2__view_ids__set_E");
   E813 : Short_Integer; pragma Import (Ada, E813, "gpr2__view_ids__vector_E");
   E818 : Short_Integer; pragma Import (Ada, E818, "gpr2__view_ids__dags_E");
   E703 : Short_Integer; pragma Import (Ada, E703, "gpr_parser_adasat_E");
   E708 : Short_Integer; pragma Import (Ada, E708, "gpr_parser_adasat__decisions_E");
   E706 : Short_Integer; pragma Import (Ada, E706, "gpr_parser_adasat__vectors_E");
   E710 : Short_Integer; pragma Import (Ada, E710, "gpr_parser_adasat__formulas_E");
   E704 : Short_Integer; pragma Import (Ada, E704, "gpr_parser_adasat__internals_E");
   E720 : Short_Integer; pragma Import (Ada, E720, "gpr_parser_adasat__builders_E");
   E701 : Short_Integer; pragma Import (Ada, E701, "gpr_parser_adasat__dpll_E");
   E712 : Short_Integer; pragma Import (Ada, E712, "gpr_parser_support__adalog_E");
   E714 : Short_Integer; pragma Import (Ada, E714, "gpr_parser_support__adalog__debug_E");
   E716 : Short_Integer; pragma Import (Ada, E716, "gpr_parser_support__adalog__logic_var_E");
   E724 : Short_Integer; pragma Import (Ada, E724, "gpr_parser_support__adalog__solver_interface_E");
   E726 : Short_Integer; pragma Import (Ada, E726, "gpr_parser_support__array_utils_E");
   E679 : Short_Integer; pragma Import (Ada, E679, "gpr_parser_support__hashes_E");
   E722 : Short_Integer; pragma Import (Ada, E722, "gpr_parser_support__images_E");
   E742 : Short_Integer; pragma Import (Ada, E742, "gpr_parser_support__packrat_E");
   E732 : Short_Integer; pragma Import (Ada, E732, "gpr_parser_support__relative_get_E");
   E628 : Short_Integer; pragma Import (Ada, E628, "gpr_parser_support__text_E");
   E693 : Short_Integer; pragma Import (Ada, E693, "gpr_parser_support__names_E");
   E618 : Short_Integer; pragma Import (Ada, E618, "gpr_parser_support__slocs_E");
   E616 : Short_Integer; pragma Import (Ada, E616, "gpr_parser_support__diagnostics_E");
   E669 : Short_Integer; pragma Import (Ada, E669, "gpr_parser_support__file_readers_E");
   E754 : Short_Integer; pragma Import (Ada, E754, "gpr2__file_readers_E");
   E683 : Short_Integer; pragma Import (Ada, E683, "gpr_parser_support__vectors_E");
   E718 : Short_Integer; pragma Import (Ada, E718, "gpr_parser_support__adalog__solver_E");
   E728 : Short_Integer; pragma Import (Ada, E728, "gpr_parser_support__cheap_sets_E");
   E746 : Short_Integer; pragma Import (Ada, E746, "gpr_parser_support__generic_bump_ptr_E");
   E744 : Short_Integer; pragma Import (Ada, E744, "gpr_parser_support__bump_ptr_E");
   E680 : Short_Integer; pragma Import (Ada, E680, "gpr_parser_support__lexical_envs_E");
   E687 : Short_Integer; pragma Import (Ada, E687, "gpr_parser_support__symbols_E");
   E730 : Short_Integer; pragma Import (Ada, E730, "gpr_parser_support__lexical_envs_impl_E");
   E734 : Short_Integer; pragma Import (Ada, E734, "gpr_parser_support__symbols__precomputed_E");
   E685 : Short_Integer; pragma Import (Ada, E685, "gpr_parser_support__token_data_handlers_E");
   E665 : Short_Integer; pragma Import (Ada, E665, "gpr_parser_support__generic_api_E");
   E677 : Short_Integer; pragma Import (Ada, E677, "gpr_parser_support__internal__analysis_E");
   E691 : Short_Integer; pragma Import (Ada, E691, "gpr_parser_support__generic_api__analysis_E");
   E675 : Short_Integer; pragma Import (Ada, E675, "gpr_parser_support__generic_api__introspection_E");
   E695 : Short_Integer; pragma Import (Ada, E695, "gpr_parser_support__internal__introspection_E");
   E649 : Short_Integer; pragma Import (Ada, E649, "gpr_parser__common_E");
   E736 : Short_Integer; pragma Import (Ada, E736, "gpr_parser__lexer_implementation_E");
   E738 : Short_Integer; pragma Import (Ada, E738, "gpr_parser__lexer_state_machine_E");
   E740 : Short_Integer; pragma Import (Ada, E740, "gpr_parser__parsers_E");
   E657 : Short_Integer; pragma Import (Ada, E657, "gpr_parser__implementation_E");
   E750 : Short_Integer; pragma Import (Ada, E750, "gpr_parser__debug_E");
   E645 : Short_Integer; pragma Import (Ada, E645, "gpr_parser__analysis_E");
   E651 : Short_Integer; pragma Import (Ada, E651, "gpr_parser__generic_api_E");
   E655 : Short_Integer; pragma Import (Ada, E655, "gpr_parser__generic_introspection_E");
   E662 : Short_Integer; pragma Import (Ada, E662, "gpr_parser__private_converters_E");
   E653 : Short_Integer; pragma Import (Ada, E653, "gpr_parser__generic_impl_E");
   E748 : Short_Integer; pragma Import (Ada, E748, "gpr_parser__public_converters_E");
   E752 : Short_Integer; pragma Import (Ada, E752, "gpr_parser__basic_ada_parser_E");
   E486 : Short_Integer; pragma Import (Ada, E486, "sax__htable_E");
   E490 : Short_Integer; pragma Import (Ada, E490, "sax__pointers_E");
   E571 : Short_Integer; pragma Import (Ada, E571, "sax__state_machines_E");
   E550 : Short_Integer; pragma Import (Ada, E550, "schema_E");
   E482 : Short_Integer; pragma Import (Ada, E482, "unicode__ccs_E");
   E506 : Short_Integer; pragma Import (Ada, E506, "unicode__ccs__iso_8859_1_E");
   E508 : Short_Integer; pragma Import (Ada, E508, "unicode__ccs__iso_8859_15_E");
   E513 : Short_Integer; pragma Import (Ada, E513, "unicode__ccs__iso_8859_2_E");
   E516 : Short_Integer; pragma Import (Ada, E516, "unicode__ccs__iso_8859_3_E");
   E518 : Short_Integer; pragma Import (Ada, E518, "unicode__ccs__iso_8859_4_E");
   E520 : Short_Integer; pragma Import (Ada, E520, "unicode__ccs__windows_1251_E");
   E525 : Short_Integer; pragma Import (Ada, E525, "unicode__ccs__windows_1252_E");
   E478 : Short_Integer; pragma Import (Ada, E478, "unicode__ces_E");
   E488 : Short_Integer; pragma Import (Ada, E488, "sax__symbols_E");
   E548 : Short_Integer; pragma Import (Ada, E548, "sax__locators_E");
   E546 : Short_Integer; pragma Import (Ada, E546, "sax__exceptions_E");
   E480 : Short_Integer; pragma Import (Ada, E480, "unicode__ces__utf32_E");
   E528 : Short_Integer; pragma Import (Ada, E528, "unicode__ces__basic_8bit_E");
   E530 : Short_Integer; pragma Import (Ada, E530, "unicode__ces__utf16_E");
   E484 : Short_Integer; pragma Import (Ada, E484, "unicode__ces__utf8_E");
   E544 : Short_Integer; pragma Import (Ada, E544, "sax__models_E");
   E542 : Short_Integer; pragma Import (Ada, E542, "sax__attributes_E");
   E492 : Short_Integer; pragma Import (Ada, E492, "sax__utils_E");
   E465 : Short_Integer; pragma Import (Ada, E465, "dom__core_E");
   E562 : Short_Integer; pragma Import (Ada, E562, "schema__date_time_E");
   E564 : Short_Integer; pragma Import (Ada, E564, "schema__decimal_E");
   E560 : Short_Integer; pragma Import (Ada, E560, "schema__simple_types_E");
   E504 : Short_Integer; pragma Import (Ada, E504, "unicode__encodings_E");
   E500 : Short_Integer; pragma Import (Ada, E500, "dom__core__nodes_E");
   E498 : Short_Integer; pragma Import (Ada, E498, "dom__core__attrs_E");
   E554 : Short_Integer; pragma Import (Ada, E554, "dom__core__character_datas_E");
   E494 : Short_Integer; pragma Import (Ada, E494, "dom__core__documents_E");
   E496 : Short_Integer; pragma Import (Ada, E496, "dom__core__elements_E");
   E534 : Short_Integer; pragma Import (Ada, E534, "input_sources_E");
   E536 : Short_Integer; pragma Import (Ada, E536, "input_sources__file_E");
   E538 : Short_Integer; pragma Import (Ada, E538, "input_sources__strings_E");
   E540 : Short_Integer; pragma Import (Ada, E540, "sax__readers_E");
   E569 : Short_Integer; pragma Import (Ada, E569, "schema__validators_E");
   E556 : Short_Integer; pragma Import (Ada, E556, "schema__readers_E");
   E558 : Short_Integer; pragma Import (Ada, E558, "schema__schema_readers_E");
   E573 : Short_Integer; pragma Import (Ada, E573, "schema__validators__xsd_grammar_E");
   E552 : Short_Integer; pragma Import (Ada, E552, "schema__dom_readers_E");
   E598 : Short_Integer; pragma Import (Ada, E598, "gpr2__build__source_base_E");
   E612 : Short_Integer; pragma Import (Ada, E612, "gpr2__build__source_base__ada_parser_E");
   E585 : Short_Integer; pragma Import (Ada, E585, "gpr2__project__registry__attribute_E");
   E583 : Short_Integer; pragma Import (Ada, E583, "gpr2__project__attr_values_E");
   E581 : Short_Integer; pragma Import (Ada, E581, "gpr2__project__attribute_E");
   E761 : Short_Integer; pragma Import (Ada, E761, "gpr2__project__attribute__set_E");
   E798 : Short_Integer; pragma Import (Ada, E798, "gpr2__project__attribute_cache_E");
   E774 : Short_Integer; pragma Import (Ada, E774, "gpr2__project__name_values_E");
   E857 : Short_Integer; pragma Import (Ada, E857, "gpr2__project__registry__attribute__description_E");
   E846 : Short_Integer; pragma Import (Ada, E846, "gpr2__project__registry__exchange_E");
   E778 : Short_Integer; pragma Import (Ada, E778, "gpr2__project__typ_E");
   E801 : Short_Integer; pragma Import (Ada, E801, "gpr2__project__typ__set_E");
   E772 : Short_Integer; pragma Import (Ada, E772, "gpr2__project__variable_E");
   E779 : Short_Integer; pragma Import (Ada, E779, "gpr2__project__variable__set_E");
   E770 : Short_Integer; pragma Import (Ada, E770, "gpr2__pack_internal_E");
   E589 : Short_Integer; pragma Import (Ada, E589, "gpr2__project__view_E");
   E822 : Short_Integer; pragma Import (Ada, E822, "gpr2__build__actions_E");
   E591 : Short_Integer; pragma Import (Ada, E591, "gpr2__build__compilation_unit_E");
   E608 : Short_Integer; pragma Import (Ada, E608, "gpr2__build__compilation_unit__maps_E");
   E596 : Short_Integer; pragma Import (Ada, E596, "gpr2__build__source_E");
   E605 : Short_Integer; pragma Import (Ada, E605, "gpr2__project__view__set_E");
   E610 : Short_Integer; pragma Import (Ada, E610, "gpr2__build__view_tables_E");
   E607 : Short_Integer; pragma Import (Ada, E607, "gpr2__build__view_db_E");
   E604 : Short_Integer; pragma Import (Ada, E604, "gpr2__build__source__sets_E");
   E593 : Short_Integer; pragma Import (Ada, E593, "gpr2__build__tree_db_E");
   E816 : Short_Integer; pragma Import (Ada, E816, "gpr2__project__view__vector_E");
   E767 : Short_Integer; pragma Import (Ada, E767, "gpr2__project_parser_E");
   E579 : Short_Integer; pragma Import (Ada, E579, "gpr2__project__configuration_E");
   E433 : Short_Integer; pragma Import (Ada, E433, "gpr2__kb_E");
   E456 : Short_Integer; pragma Import (Ada, E456, "gpr2__kb__compiler_iterator_E");
   E532 : Short_Integer; pragma Import (Ada, E532, "gpr2__kb__embedded_E");
   E462 : Short_Integer; pragma Import (Ada, E462, "gpr2__kb__parsing_E");
   E406 : Short_Integer; pragma Import (Ada, E406, "gpr2__options_E");
   E811 : Short_Integer; pragma Import (Ada, E811, "gpr2__project_parser__create_E");
   E783 : Short_Integer; pragma Import (Ada, E783, "gpr2__project_parser__registry_E");
   E802 : Short_Integer; pragma Import (Ada, E802, "gpr2__project_parser__set_E");
   E792 : Short_Integer; pragma Import (Ada, E792, "gpr2__view_internal_E");
   E758 : Short_Integer; pragma Import (Ada, E758, "gpr2__tree_internal_E");
   E815 : Short_Integer; pragma Import (Ada, E815, "gpr2__project__tree_E");
   E809 : Short_Integer; pragma Import (Ada, E809, "gpr2__tree_internal__view_builder_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E792 := E792 - 1;
      E758 := E758 - 1;
      E809 := E809 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "gpr2__tree_internal__view_builder__finalize_spec");
      begin
         F1;
      end;
      E767 := E767 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "gpr2__project__view__finalize_body");
      begin
         E589 := E589 - 1;
         F2;
      end;
      E815 := E815 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "gpr2__project__tree__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gpr2__build__view_tables__finalize_body");
      begin
         E610 := E610 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gpr2__build__tree_db__finalize_body");
      begin
         E593 := E593 - 1;
         F5;
      end;
      E591 := E591 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gpr2__tree_internal__finalize_spec");
      begin
         F6;
      end;
      E579 := E579 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gpr2__view_internal__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gpr2__project_parser__set__finalize_spec");
      begin
         E802 := E802 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gpr2__project_parser__registry__finalize_body");
      begin
         E783 := E783 - 1;
         F9;
      end;
      E406 := E406 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gpr2__options__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gpr2__kb__finalize_body");
      begin
         E433 := E433 - 1;
         F11;
      end;
      E532 := E532 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gpr2__kb__embedded__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gpr2__kb__compiler_iterator__finalize_body");
      begin
         E456 := E456 - 1;
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gpr2__kb__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gpr2__project__configuration__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gpr2__project_parser__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gpr2__project__view__vector__finalize_spec");
      begin
         E816 := E816 - 1;
         F17;
      end;
      E607 := E607 - 1;
      E598 := E598 - 1;
      E604 := E604 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gpr2__build__tree_db__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "gpr2__build__source__sets__finalize_spec");
      begin
         F19;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gpr2__build__view_db__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "gpr2__build__view_tables__finalize_spec");
      begin
         F21;
      end;
      declare
         procedure F22;
         pragma Import (Ada, F22, "gpr2__project__view__set__finalize_spec");
      begin
         E605 := E605 - 1;
         F22;
      end;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gpr2__project__registry__attribute__finalize_body");
      begin
         E585 := E585 - 1;
         F23;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gpr2__build__source__finalize_spec");
      begin
         E596 := E596 - 1;
         F24;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "gpr2__build__compilation_unit__maps__finalize_spec");
      begin
         E608 := E608 - 1;
         F25;
      end;
      declare
         procedure F26;
         pragma Import (Ada, F26, "gpr2__build__compilation_unit__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "gpr2__project__view__finalize_spec");
      begin
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "gpr2__pack_internal__finalize_spec");
      begin
         E770 := E770 - 1;
         F28;
      end;
      declare
         procedure F29;
         pragma Import (Ada, F29, "gpr2__project__variable__set__finalize_spec");
      begin
         E779 := E779 - 1;
         F29;
      end;
      E772 := E772 - 1;
      declare
         procedure F30;
         pragma Import (Ada, F30, "gpr2__project__variable__finalize_spec");
      begin
         F30;
      end;
      declare
         procedure F31;
         pragma Import (Ada, F31, "gpr2__project__typ__set__finalize_spec");
      begin
         E801 := E801 - 1;
         F31;
      end;
      E778 := E778 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "gpr2__project__typ__finalize_spec");
      begin
         F32;
      end;
      E857 := E857 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "gpr2__project__registry__attribute__description__finalize_spec");
      begin
         F33;
      end;
      E774 := E774 - 1;
      declare
         procedure F34;
         pragma Import (Ada, F34, "gpr2__project__name_values__finalize_spec");
      begin
         F34;
      end;
      E798 := E798 - 1;
      declare
         procedure F35;
         pragma Import (Ada, F35, "gpr2__project__attribute_cache__finalize_spec");
      begin
         F35;
      end;
      declare
         procedure F36;
         pragma Import (Ada, F36, "gpr2__project__attribute__set__finalize_body");
      begin
         E761 := E761 - 1;
         F36;
      end;
      declare
         procedure F37;
         pragma Import (Ada, F37, "gpr2__project__attribute__set__finalize_spec");
      begin
         F37;
      end;
      E581 := E581 - 1;
      declare
         procedure F38;
         pragma Import (Ada, F38, "gpr2__project__attribute__finalize_spec");
      begin
         F38;
      end;
      E583 := E583 - 1;
      declare
         procedure F39;
         pragma Import (Ada, F39, "gpr2__project__attr_values__finalize_spec");
      begin
         F39;
      end;
      declare
         procedure F40;
         pragma Import (Ada, F40, "gpr2__project__registry__attribute__finalize_spec");
      begin
         F40;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "gpr2__build__source_base__finalize_spec");
      begin
         F41;
      end;
      E552 := E552 - 1;
      declare
         procedure F42;
         pragma Import (Ada, F42, "schema__dom_readers__finalize_spec");
      begin
         F42;
      end;
      E569 := E569 - 1;
      E556 := E556 - 1;
      E558 := E558 - 1;
      declare
         procedure F43;
         pragma Import (Ada, F43, "schema__schema_readers__finalize_spec");
      begin
         F43;
      end;
      declare
         procedure F44;
         pragma Import (Ada, F44, "schema__readers__finalize_spec");
      begin
         F44;
      end;
      declare
         procedure F45;
         pragma Import (Ada, F45, "schema__validators__finalize_spec");
      begin
         F45;
      end;
      E540 := E540 - 1;
      declare
         procedure F46;
         pragma Import (Ada, F46, "sax__readers__finalize_spec");
      begin
         F46;
      end;
      E538 := E538 - 1;
      declare
         procedure F47;
         pragma Import (Ada, F47, "input_sources__strings__finalize_spec");
      begin
         F47;
      end;
      E536 := E536 - 1;
      declare
         procedure F48;
         pragma Import (Ada, F48, "input_sources__file__finalize_spec");
      begin
         F48;
      end;
      E534 := E534 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "input_sources__finalize_spec");
      begin
         F49;
      end;
      E465 := E465 - 1;
      declare
         procedure F50;
         pragma Import (Ada, F50, "dom__core__finalize_spec");
      begin
         F50;
      end;
      E492 := E492 - 1;
      declare
         procedure F51;
         pragma Import (Ada, F51, "sax__utils__finalize_spec");
      begin
         F51;
      end;
      E542 := E542 - 1;
      declare
         procedure F52;
         pragma Import (Ada, F52, "sax__attributes__finalize_spec");
      begin
         F52;
      end;
      E546 := E546 - 1;
      declare
         procedure F53;
         pragma Import (Ada, F53, "sax__exceptions__finalize_spec");
      begin
         F53;
      end;
      E488 := E488 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "sax__symbols__finalize_spec");
      begin
         F54;
      end;
      E490 := E490 - 1;
      declare
         procedure F55;
         pragma Import (Ada, F55, "sax__pointers__finalize_spec");
      begin
         F55;
      end;
      E655 := E655 - 1;
      E645 := E645 - 1;
      declare
         procedure F56;
         pragma Import (Ada, F56, "gpr_parser__public_converters__finalize_body");
      begin
         E748 := E748 - 1;
         F56;
      end;
      declare
         procedure F57;
         pragma Import (Ada, F57, "gpr_parser__public_converters__finalize_spec");
      begin
         F57;
      end;
      declare
         procedure F58;
         pragma Import (Ada, F58, "gpr_parser__parsers__finalize_body");
      begin
         E740 := E740 - 1;
         F58;
      end;
      declare
         procedure F59;
         pragma Import (Ada, F59, "gpr_parser__implementation__finalize_body");
      begin
         E657 := E657 - 1;
         F59;
      end;
      declare
         procedure F60;
         pragma Import (Ada, F60, "gpr_parser__generic_introspection__finalize_spec");
      begin
         F60;
      end;
      declare
         procedure F61;
         pragma Import (Ada, F61, "gpr_parser__analysis__finalize_spec");
      begin
         F61;
      end;
      declare
         procedure F62;
         pragma Import (Ada, F62, "gpr_parser__implementation__finalize_spec");
      begin
         F62;
      end;
      E695 := E695 - 1;
      E677 := E677 - 1;
      E675 := E675 - 1;
      E691 := E691 - 1;
      declare
         procedure F63;
         pragma Import (Ada, F63, "gpr_parser_support__internal__introspection__finalize_spec");
      begin
         F63;
      end;
      declare
         procedure F64;
         pragma Import (Ada, F64, "gpr_parser_support__generic_api__introspection__finalize_spec");
      begin
         F64;
      end;
      declare
         procedure F65;
         pragma Import (Ada, F65, "gpr_parser_support__generic_api__analysis__finalize_spec");
      begin
         F65;
      end;
      declare
         procedure F66;
         pragma Import (Ada, F66, "gpr_parser_support__internal__analysis__finalize_spec");
      begin
         F66;
      end;
      E685 := E685 - 1;
      declare
         procedure F67;
         pragma Import (Ada, F67, "gpr_parser_support__token_data_handlers__finalize_spec");
      begin
         F67;
      end;
      E687 := E687 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "gpr_parser_support__symbols__finalize_spec");
      begin
         F68;
      end;
      declare
         procedure F69;
         pragma Import (Ada, F69, "gpr_parser_support__lexical_envs__finalize_spec");
      begin
         E680 := E680 - 1;
         F69;
      end;
      E744 := E744 - 1;
      declare
         procedure F70;
         pragma Import (Ada, F70, "gpr_parser_support__bump_ptr__finalize_spec");
      begin
         F70;
      end;
      declare
         procedure F71;
         pragma Import (Ada, F71, "gpr2__file_readers__finalize_body");
      begin
         E754 := E754 - 1;
         F71;
      end;
      declare
         procedure F72;
         pragma Import (Ada, F72, "gpr2__file_readers__finalize_spec");
      begin
         F72;
      end;
      E669 := E669 - 1;
      declare
         procedure F73;
         pragma Import (Ada, F73, "gpr_parser_support__file_readers__finalize_spec");
      begin
         F73;
      end;
      E616 := E616 - 1;
      declare
         procedure F74;
         pragma Import (Ada, F74, "gpr_parser_support__diagnostics__finalize_spec");
      begin
         F74;
      end;
      E720 := E720 - 1;
      declare
         procedure F75;
         pragma Import (Ada, F75, "gpr_parser_adasat__builders__finalize_spec");
      begin
         F75;
      end;
      declare
         procedure F76;
         pragma Import (Ada, F76, "gpr_parser_adasat__internals__finalize_spec");
      begin
         E704 := E704 - 1;
         F76;
      end;
      E710 := E710 - 1;
      declare
         procedure F77;
         pragma Import (Ada, F77, "gpr_parser_adasat__formulas__finalize_spec");
      begin
         F77;
      end;
      declare
         procedure F78;
         pragma Import (Ada, F78, "gpr2__view_ids__dags__finalize_body");
      begin
         E818 := E818 - 1;
         F78;
      end;
      declare
         procedure F79;
         pragma Import (Ada, F79, "gpr2__view_ids__dags__finalize_spec");
      begin
         F79;
      end;
      E813 := E813 - 1;
      declare
         procedure F80;
         pragma Import (Ada, F80, "gpr2__view_ids__vector__finalize_spec");
      begin
         F80;
      end;
      E807 := E807 - 1;
      declare
         procedure F81;
         pragma Import (Ada, F81, "gpr2__view_ids__set__finalize_spec");
      begin
         F81;
      end;
      declare
         procedure F82;
         pragma Import (Ada, F82, "gpr2__view_base_internal__finalize_spec");
      begin
         E803 := E803 - 1;
         F82;
      end;
      E826 := E826 - 1;
      declare
         procedure F83;
         pragma Import (Ada, F83, "gpr2__utils__hash__finalize_spec");
      begin
         F83;
      end;
      E859 := E859 - 1;
      declare
         procedure F84;
         pragma Import (Ada, F84, "gpr2__project__registry__pack__description__finalize_spec");
      begin
         F84;
      end;
      declare
         procedure F85;
         pragma Import (Ada, F85, "gpr2__project__registry__pack__finalize_body");
      begin
         E587 := E587 - 1;
         F85;
      end;
      declare
         procedure F86;
         pragma Import (Ada, F86, "gpr2__project__import__set__finalize_body");
      begin
         E765 := E765 - 1;
         F86;
      end;
      declare
         procedure F87;
         pragma Import (Ada, F87, "gpr2__project__import__set__finalize_spec");
      begin
         F87;
      end;
      E763 := E763 - 1;
      declare
         procedure F88;
         pragma Import (Ada, F88, "gpr2__project__import__finalize_spec");
      begin
         F88;
      end;
      E756 := E756 - 1;
      declare
         procedure F89;
         pragma Import (Ada, F89, "gpr2__project__attribute_index__finalize_spec");
      begin
         F89;
      end;
      E252 := E252 - 1;
      declare
         procedure F90;
         pragma Import (Ada, F90, "gpr2__build__ali_parser__finalize_spec");
      begin
         F90;
      end;
      declare
         procedure F91;
         pragma Import (Ada, F91, "gpr2__log__finalize_body");
      begin
         E394 := E394 - 1;
         F91;
      end;
      declare
         procedure F92;
         pragma Import (Ada, F92, "gpr2__log__finalize_spec");
      begin
         F92;
      end;
      E431 := E431 - 1;
      declare
         procedure F93;
         pragma Import (Ada, F93, "gpr2__context__finalize_spec");
      begin
         F93;
      end;
      declare
         procedure F94;
         pragma Import (Ada, F94, "gpr2__build__unit_info__list__finalize_body");
      begin
         E602 := E602 - 1;
         F94;
      end;
      declare
         procedure F95;
         pragma Import (Ada, F95, "gpr2__build__unit_info__list__finalize_spec");
      begin
         F95;
      end;
      E600 := E600 - 1;
      declare
         procedure F96;
         pragma Import (Ada, F96, "gpr2__build__unit_info__finalize_spec");
      begin
         F96;
      end;
      E398 := E398 - 1;
      declare
         procedure F97;
         pragma Import (Ada, F97, "gpr2__containers__finalize_spec");
      begin
         F97;
      end;
      declare
         procedure F98;
         pragma Import (Ada, F98, "gpr2__source_reference__value__finalize_spec");
      begin
         E402 := E402 - 1;
         F98;
      end;
      E776 := E776 - 1;
      declare
         procedure F99;
         pragma Import (Ada, F99, "gpr2__source_reference__identifier__finalize_spec");
      begin
         F99;
      end;
      declare
         procedure F100;
         pragma Import (Ada, F100, "gpr2__source_reference__pack__finalize_spec");
      begin
         E780 := E780 - 1;
         F100;
      end;
      declare
         procedure F101;
         pragma Import (Ada, F101, "gpr2__source_reference__attribute__finalize_spec");
      begin
         E790 := E790 - 1;
         F101;
      end;
      declare
         procedure F102;
         pragma Import (Ada, F102, "gpr2__message__reporter__finalize_body");
      begin
         E396 := E396 - 1;
         F102;
      end;
      declare
         procedure F103;
         pragma Import (Ada, F103, "gpr2__message__reporter__finalize_spec");
      begin
         F103;
      end;
      E298 := E298 - 1;
      declare
         procedure F104;
         pragma Import (Ada, F104, "gpr2__message__finalize_spec");
      begin
         F104;
      end;
      E349 := E349 - 1;
      declare
         procedure F105;
         pragma Import (Ada, F105, "gpr2__source_reference__finalize_spec");
      begin
         F105;
      end;
      E426 := E426 - 1;
      declare
         procedure F106;
         pragma Import (Ada, F106, "gpr2__path_name__set__finalize_spec");
      begin
         F106;
      end;
      E351 := E351 - 1;
      declare
         procedure F107;
         pragma Import (Ada, F107, "gpr2__path_name__finalize_spec");
      begin
         F107;
      end;
      E414 := E414 - 1;
      declare
         procedure F108;
         pragma Import (Ada, F108, "gpr2__environment__finalize_spec");
      begin
         F108;
      end;
      E157 := E157 - 1;
      declare
         procedure F109;
         pragma Import (Ada, F109, "gpr2__finalize_spec");
      begin
         F109;
      end;
      declare
         procedure F110;
         pragma Import (Ada, F110, "gnatcoll__traces__finalize_body");
      begin
         E435 := E435 - 1;
         F110;
      end;
      declare
         procedure F111;
         pragma Import (Ada, F111, "gnatcoll__traces__finalize_spec");
      begin
         F111;
      end;
      E368 := E368 - 1;
      declare
         procedure F112;
         pragma Import (Ada, F112, "gnatcoll__vfs__finalize_spec");
      begin
         F112;
      end;
      E375 := E375 - 1;
      declare
         procedure F113;
         pragma Import (Ada, F113, "gnatcoll__io__remote__finalize_spec");
      begin
         F113;
      end;
      declare
         procedure F114;
         pragma Import (Ada, F114, "gnatcoll__remote__finalize_spec");
      begin
         E384 := E384 - 1;
         F114;
      end;
      E379 := E379 - 1;
      declare
         procedure F115;
         pragma Import (Ada, F115, "gnatcoll__io__native__finalize_spec");
      begin
         F115;
      end;
      E372 := E372 - 1;
      declare
         procedure F116;
         pragma Import (Ada, F116, "gnatcoll__io__finalize_spec");
      begin
         F116;
      end;
      E450 := E450 - 1;
      declare
         procedure F117;
         pragma Import (Ada, F117, "gnatcoll__terminal__finalize_spec");
      begin
         F117;
      end;
      E848 := E848 - 1;
      declare
         procedure F118;
         pragma Import (Ada, F118, "gnatcoll__json__finalize_spec");
      begin
         F118;
      end;
      E254 := E254 - 1;
      declare
         procedure F119;
         pragma Import (Ada, F119, "gnatcoll__buffer__finalize_spec");
      begin
         F119;
      end;
      E269 := E269 - 1;
      declare
         procedure F120;
         pragma Import (Ada, F120, "gnatcoll__strings__finalize_spec");
      begin
         F120;
      end;
      E828 := E828 - 1;
      declare
         procedure F121;
         pragma Import (Ada, F121, "gnatcoll__file_indexes__finalize_spec");
      begin
         F121;
      end;
      E416 := E416 - 1;
      declare
         procedure F122;
         pragma Import (Ada, F122, "gnatcoll__os__process__finalize_spec");
      begin
         F122;
      end;
      E844 := E844 - 1;
      declare
         procedure F123;
         pragma Import (Ada, F123, "gnatcoll__hash__blake3__finalize_spec");
      begin
         F123;
      end;
      E275 := E275 - 1;
      declare
         procedure F124;
         pragma Import (Ada, F124, "gnatcoll__refcount__finalize_spec");
      begin
         F124;
      end;
      declare
         procedure F125;
         pragma Import (Ada, F125, "gnatcoll__memory__finalize_body");
      begin
         E439 := E439 - 1;
         F125;
      end;
      declare
         procedure F126;
         pragma Import (Ada, F126, "gnatcoll__directed_graph__finalize_body");
      begin
         E595 := E595 - 1;
         F126;
      end;
      declare
         procedure F127;
         pragma Import (Ada, F127, "gnatcoll__directed_graph__finalize_spec");
      begin
         F127;
      end;
      E698 := E698 - 1;
      declare
         procedure F128;
         pragma Import (Ada, F128, "gnatcoll__gmp__integers__finalize_spec");
      begin
         F128;
      end;
      declare
         procedure F129;
         pragma Import (Ada, F129, "ada__directories__finalize_body");
      begin
         E159 := E159 - 1;
         F129;
      end;
      declare
         procedure F130;
         pragma Import (Ada, F130, "ada__directories__finalize_spec");
      begin
         F130;
      end;
      E183 := E183 - 1;
      declare
         procedure F131;
         pragma Import (Ada, F131, "system__regexp__finalize_spec");
      begin
         F131;
      end;
      E300 := E300 - 1;
      declare
         procedure F132;
         pragma Import (Ada, F132, "gnat__formatted_string__finalize_spec");
      begin
         F132;
      end;
      E236 := E236 - 1;
      declare
         procedure F133;
         pragma Import (Ada, F133, "gnat__expect__finalize_spec");
      begin
         F133;
      end;
      E245 := E245 - 1;
      declare
         procedure F134;
         pragma Import (Ada, F134, "system__pool_global__finalize_spec");
      begin
         F134;
      end;
      declare
         procedure F135;
         pragma Import (Ada, F135, "gnat__debug_pools__finalize_body");
      begin
         E441 := E441 - 1;
         F135;
      end;
      declare
         procedure F136;
         pragma Import (Ada, F136, "gnat__debug_pools__finalize_spec");
      begin
         F136;
      end;
      E400 := E400 - 1;
      declare
         procedure F137;
         pragma Import (Ada, F137, "gnat__string_split__finalize_spec");
      begin
         F137;
      end;
      E837 := E837 - 1;
      declare
         procedure F138;
         pragma Import (Ada, F138, "gnat__sha256__finalize_spec");
      begin
         F138;
      end;
      E833 := E833 - 1;
      declare
         procedure F139;
         pragma Import (Ada, F139, "gnat__sha1__finalize_spec");
      begin
         F139;
      end;
      E359 := E359 - 1;
      declare
         procedure F140;
         pragma Import (Ada, F140, "gnat__md5__finalize_spec");
      begin
         F140;
      end;
      E150 := E150 - 1;
      declare
         procedure F141;
         pragma Import (Ada, F141, "ada__text_io__finalize_spec");
      begin
         F141;
      end;
      E639 := E639 - 1;
      declare
         procedure F142;
         pragma Import (Ada, F142, "ada__strings__wide_wide_unbounded__finalize_spec");
      begin
         F142;
      end;
      E622 := E622 - 1;
      declare
         procedure F143;
         pragma Import (Ada, F143, "ada__strings__wide_wide_maps__finalize_spec");
      begin
         F143;
      end;
      E119 := E119 - 1;
      declare
         procedure F144;
         pragma Import (Ada, F144, "ada__strings__unbounded__finalize_spec");
      begin
         F144;
      end;
      E187 := E187 - 1;
      declare
         procedure F145;
         pragma Import (Ada, F145, "system__storage_pools__subpools__finalize_spec");
      begin
         F145;
      end;
      E353 := E353 - 1;
      declare
         procedure F146;
         pragma Import (Ada, F146, "ada__streams__stream_io__finalize_spec");
      begin
         F146;
      end;
      declare
         procedure F147;
         pragma Import (Ada, F147, "system__file_io__finalize_body");
      begin
         E154 := E154 - 1;
         F147;
      end;
      E127 := E127 - 1;
      declare
         procedure F148;
         pragma Import (Ada, F148, "system__finalization_primitives__finalize_spec");
      begin
         F148;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");
      Interrupts_Default_To_System : Integer;
      pragma Import (C, Interrupts_Default_To_System, "__gl_interrupts_default_to_system");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      procedure Tasking_Runtime_Initialize;
      pragma Import (C, Tasking_Runtime_Initialize, "__gnat_tasking_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);
      Tasking_Runtime_Initialize;

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E022 := E022 + 1;
      Ada.Containers'Elab_Spec;
      E038 := E038 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Numerics'Elab_Spec;
      E029 := E029 + 1;
      Ada.Strings'Elab_Spec;
      E053 := E053 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E055 := E055 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E058 := E058 + 1;
      Interfaces.C'Elab_Spec;
      E043 := E043 + 1;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.Object_Reader'Elab_Spec;
      E084 := E084 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E048 := E048 + 1;
      System.Os_Lib'Elab_Body;
      E073 := E073 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E018 := E018 + 1;
      E011 := E011 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E037 := E037 + 1;
      E006 := E006 + 1;
      Ada.Assertions'Elab_Spec;
      E197 := E197 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E105 := E105 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E113 := E113 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E103 := E103 + 1;
      Gnat'Elab_Spec;
      E192 := E192 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E220 := E220 + 1;
      Ada.Streams'Elab_Spec;
      E130 := E130 + 1;
      System.File_Control_Block'Elab_Spec;
      E155 := E155 + 1;
      System.Finalization_Root'Elab_Spec;
      E136 := E136 + 1;
      Ada.Finalization'Elab_Spec;
      E128 := E128 + 1;
      System.Finalization_Primitives'Elab_Spec;
      E127 := E127 + 1;
      System.File_Io'Elab_Body;
      E154 := E154 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E353 := E353 + 1;
      System.Storage_Pools'Elab_Spec;
      E185 := E185 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E187 := E187 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E119 := E119 + 1;
      Ada.Strings.Wide_Wide_Maps'Elab_Spec;
      E622 := E622 + 1;
      Ada.Strings.Wide_Wide_Unbounded'Elab_Spec;
      E639 := E639 + 1;
      System.Task_Info'Elab_Spec;
      E292 := E292 + 1;
      System.Regpat'Elab_Spec;
      E241 := E241 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E161 := E161 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E673 := E673 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E167 := E167 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E150 := E150 + 1;
      Ada.Text_Io.Text_Streams'Elab_Spec;
      E502 := E502 + 1;
      E671 := E671 + 1;
      Gnat.Calendar'Elab_Spec;
      E222 := E222 + 1;
      Gnat.Directory_Operations'Elab_Spec;
      Gnat.Directory_Operations'Elab_Body;
      E377 := E377 + 1;
      E567 := E567 + 1;
      E361 := E361 + 1;
      E363 := E363 + 1;
      Gnat.Md5'Elab_Spec;
      E359 := E359 + 1;
      E835 := E835 + 1;
      E841 := E841 + 1;
      E839 := E839 + 1;
      Gnat.Sha1'Elab_Spec;
      E833 := E833 + 1;
      Gnat.Sha256'Elab_Spec;
      E837 := E837 + 1;
      Gnat.String_Split'Elab_Spec;
      E400 := E400 + 1;
      System.Checked_Pools'Elab_Spec;
      E446 := E446 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E286 := E286 + 1;
      Gnat.Calendar.Time_Io'Elab_Spec;
      E225 := E225 + 1;
      Gnat.Debug_Pools'Elab_Spec;
      Gnat.Debug_Pools'Elab_Body;
      E441 := E441 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E245 := E245 + 1;
      Gnat.Expect'Elab_Spec;
      Gnat.Expect'Elab_Body;
      E236 := E236 + 1;
      Gnat.Formatted_String'Elab_Spec;
      Gnat.Formatted_String'Elab_Body;
      E300 := E300 + 1;
      System.Random_Seed'Elab_Body;
      E392 := E392 + 1;
      System.Regexp'Elab_Spec;
      System.Regexp'Elab_Body;
      E183 := E183 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E159 := E159 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E785 := E785 + 1;
      GNATCOLL.GMP.INTEGERS'ELAB_SPEC;
      E698 := E698 + 1;
      Gpr_Parser_Support.Errors'Elab_Spec;
      E666 := E666 + 1;
      Unicode'Elab_Body;
      E469 := E469 + 1;
      E271 := E271 + 1;
      GNATCOLL.DIRECTED_GRAPH'ELAB_SPEC;
      GNATCOLL.DIRECTED_GRAPH'ELAB_BODY;
      E595 := E595 + 1;
      GNATCOLL.MEMORY'ELAB_BODY;
      E439 := E439 + 1;
      GNATCOLL.OS'ELAB_SPEC;
      E232 := E232 + 1;
      E277 := E277 + 1;
      GNATCOLL.REFCOUNT'ELAB_SPEC;
      E275 := E275 + 1;
      E231 := E231 + 1;
      E261 := E261 + 1;
      GNATCOLL.HASH.BLAKE3'ELAB_SPEC;
      E844 := E844 + 1;
      E256 := E256 + 1;
      E794 := E794 + 1;
      E424 := E424 + 1;
      E422 := E422 + 1;
      GNATCOLL.OS.PROCESS'ELAB_SPEC;
      E416 := E416 + 1;
      E273 := E273 + 1;
      GNATCOLL.OS.FSUTIL'ELAB_BODY;
      E830 := E830 + 1;
      GNATCOLL.FILE_INDEXES'ELAB_SPEC;
      E828 := E828 + 1;
      GNATCOLL.STRINGS'ELAB_SPEC;
      GNATCOLL.STRINGS'ELAB_BODY;
      E269 := E269 + 1;
      GNATCOLL.MMAP'ELAB_SPEC;
      E267 := E267 + 1;
      E265 := E265 + 1;
      GNATCOLL.BUFFER'ELAB_SPEC;
      GNATCOLL.BUFFER'ELAB_BODY;
      E254 := E254 + 1;
      GNATCOLL.JSON'ELAB_SPEC;
      E850 := E850 + 1;
      GNATCOLL.JSON'ELAB_BODY;
      E848 := E848 + 1;
      GNATCOLL.TEMPLATES'ELAB_SPEC;
      E448 := E448 + 1;
      GNATCOLL.TERMINAL'ELAB_SPEC;
      GNATCOLL.TERMINAL'ELAB_BODY;
      E450 := E450 + 1;
      E410 := E410 + 1;
      E214 := E214 + 1;
      GNATCOLL.IO'ELAB_SPEC;
      GNATCOLL.IO'ELAB_BODY;
      E372 := E372 + 1;
      GNATCOLL.PATH'ELAB_SPEC;
      GNATCOLL.PATH'ELAB_BODY;
      E381 := E381 + 1;
      GNATCOLL.IO.NATIVE'ELAB_SPEC;
      GNATCOLL.IO.NATIVE'ELAB_BODY;
      E379 := E379 + 1;
      GNATCOLL.REMOTE'ELAB_SPEC;
      E384 := E384 + 1;
      GNATCOLL.REMOTE.DB'ELAB_SPEC;
      E388 := E388 + 1;
      GNATCOLL.IO.REMOTE'ELAB_SPEC;
      E383 := E383 + 1;
      E386 := E386 + 1;
      GNATCOLL.IO.REMOTE'ELAB_BODY;
      E375 := E375 + 1;
      GNATCOLL.VFS'ELAB_SPEC;
      GNATCOLL.VFS'ELAB_BODY;
      E368 := E368 + 1;
      GNATCOLL.TRACES'ELAB_SPEC;
      GNATCOLL.TRACES'ELAB_BODY;
      E435 := E435 + 1;
      GNATCOLL.ICONV'ELAB_SPEC;
      GNATCOLL.ICONV'ELAB_BODY;
      E635 := E635 + 1;
      GNATCOLL.VFS_UTILS'ELAB_SPEC;
      E454 := E454 + 1;
      GPR2'ELAB_SPEC;
      GPR2'ELAB_BODY;
      E157 := E157 + 1;
      GPR2.ENVIRONMENT'ELAB_SPEC;
      GPR2.ENVIRONMENT'ELAB_BODY;
      E414 := E414 + 1;
      GPR2.PATH_NAME'ELAB_SPEC;
      GPR2.PATH_NAME'ELAB_BODY;
      E351 := E351 + 1;
      GPR2.PATH_NAME.SET'ELAB_SPEC;
      E426 := E426 + 1;
      GPR2.SOURCE_REFERENCE'ELAB_SPEC;
      GPR2.SOURCE_REFERENCE'ELAB_BODY;
      E349 := E349 + 1;
      GPR2.MESSAGE'ELAB_SPEC;
      GPR2.MESSAGE'ELAB_BODY;
      E298 := E298 + 1;
      GPR2.MESSAGE.REPORTER'ELAB_SPEC;
      GPR2.MESSAGE.REPORTER'ELAB_BODY;
      E396 := E396 + 1;
      GPR2.SOURCE_REFERENCE.ATTRIBUTE'ELAB_SPEC;
      E790 := E790 + 1;
      GPR2.SOURCE_REFERENCE.PACK'ELAB_SPEC;
      E780 := E780 + 1;
      E404 := E404 + 1;
      GPR2.SOURCE_REFERENCE.IDENTIFIER'ELAB_SPEC;
      E776 := E776 + 1;
      GPR2.SOURCE_REFERENCE.VALUE'ELAB_SPEC;
      E402 := E402 + 1;
      GPR2.CONTAINERS'ELAB_SPEC;
      E398 := E398 + 1;
      GPR2.BUILD.UNIT_INFO'ELAB_SPEC;
      GPR2.BUILD.UNIT_INFO'ELAB_BODY;
      E600 := E600 + 1;
      GPR2.BUILD.UNIT_INFO.LIST'ELAB_SPEC;
      GPR2.BUILD.UNIT_INFO.LIST'ELAB_BODY;
      E602 := E602 + 1;
      GPR2.CONTEXT'ELAB_SPEC;
      GPR2.CONTEXT'ELAB_BODY;
      E431 := E431 + 1;
      E769 := E769 + 1;
      GPR2.LOG'ELAB_SPEC;
      GPR2.LOG'ELAB_BODY;
      E394 := E394 + 1;
      GPR2.BUILD.ALI_PARSER'ELAB_SPEC;
      GPR2.BUILD.ALI_PARSER'ELAB_BODY;
      E252 := E252 + 1;
      GPR2.PROJECT'ELAB_SPEC;
      E408 := E408 + 1;
      GPR2.PROJECT.ATTRIBUTE_INDEX'ELAB_SPEC;
      GPR2.PROJECT.ATTRIBUTE_INDEX'ELAB_BODY;
      E756 := E756 + 1;
      GPR2.PROJECT.IMPORT'ELAB_SPEC;
      GPR2.PROJECT.IMPORT'ELAB_BODY;
      E763 := E763 + 1;
      GPR2.PROJECT.IMPORT.SET'ELAB_SPEC;
      GPR2.PROJECT.IMPORT.SET'ELAB_BODY;
      E765 := E765 + 1;
      GPR2.PROJECT.REGISTRY.PACK'ELAB_SPEC;
      GPR2.PROJECT.REGISTRY.PACK'ELAB_BODY;
      E587 := E587 + 1;
      GPR2.PROJECT.REGISTRY.PACK.DESCRIPTION'ELAB_SPEC;
      GPR2.PROJECT.REGISTRY.PACK.DESCRIPTION'ELAB_BODY;
      E859 := E859 + 1;
      GPR2.UTILS.HASH'ELAB_SPEC;
      E826 := E826 + 1;
      GPR2.BUILD.ARTIFACTS'ELAB_SPEC;
      E823 := E823 + 1;
      GPR2.VIEW_BASE_INTERNAL'ELAB_SPEC;
      E803 := E803 + 1;
      GPR2.VIEW_IDS'ELAB_SPEC;
      E805 := E805 + 1;
      GPR2.VIEW_IDS.SET'ELAB_SPEC;
      E807 := E807 + 1;
      GPR2.VIEW_IDS.VECTOR'ELAB_SPEC;
      E813 := E813 + 1;
      GPR2.VIEW_IDS.DAGS'ELAB_SPEC;
      GPR2.VIEW_IDS.DAGS'ELAB_BODY;
      E818 := E818 + 1;
      E703 := E703 + 1;
      E708 := E708 + 1;
      E706 := E706 + 1;
      Gpr_Parser_Adasat.Formulas'Elab_Spec;
      E710 := E710 + 1;
      Gpr_Parser_Adasat.Internals'Elab_Spec;
      E704 := E704 + 1;
      Gpr_Parser_Adasat.Builders'Elab_Spec;
      Gpr_Parser_Adasat.Builders'Elab_Body;
      E720 := E720 + 1;
      E701 := E701 + 1;
      Gpr_Parser_Support.Adalog'Elab_Spec;
      E712 := E712 + 1;
      E714 := E714 + 1;
      E716 := E716 + 1;
      E724 := E724 + 1;
      E726 := E726 + 1;
      E679 := E679 + 1;
      E722 := E722 + 1;
      E742 := E742 + 1;
      E732 := E732 + 1;
      Gpr_Parser_Support.Text'Elab_Spec;
      E628 := E628 + 1;
      Gpr_Parser_Support.Names'Elab_Spec;
      E693 := E693 + 1;
      E618 := E618 + 1;
      Gpr_Parser_Support.Diagnostics'Elab_Spec;
      E616 := E616 + 1;
      Gpr_Parser_Support.File_Readers'Elab_Spec;
      E669 := E669 + 1;
      GPR2.FILE_READERS'ELAB_SPEC;
      GPR2.FILE_READERS'ELAB_BODY;
      E754 := E754 + 1;
      E683 := E683 + 1;
      E718 := E718 + 1;
      E728 := E728 + 1;
      E746 := E746 + 1;
      Gpr_Parser_Support.Bump_Ptr'Elab_Spec;
      E744 := E744 + 1;
      Gpr_Parser_Support.Lexical_Envs'Elab_Spec;
      E680 := E680 + 1;
      Gpr_Parser_Support.Symbols'Elab_Spec;
      E687 := E687 + 1;
      E730 := E730 + 1;
      E734 := E734 + 1;
      Gpr_Parser_Support.Token_Data_Handlers'Elab_Spec;
      E685 := E685 + 1;
      Gpr_Parser_Support.Generic_Api'Elab_Spec;
      Gpr_Parser_Support.Internal.Analysis'Elab_Spec;
      Gpr_Parser_Support.Generic_Api.Analysis'Elab_Spec;
      Gpr_Parser_Support.Generic_Api.Introspection'Elab_Spec;
      Gpr_Parser_Support.Internal.Introspection'Elab_Spec;
      E665 := E665 + 1;
      Gpr_Parser_Support.Generic_Api.Analysis'Elab_Body;
      E691 := E691 + 1;
      Gpr_Parser_Support.Generic_Api.Introspection'Elab_Body;
      E675 := E675 + 1;
      E677 := E677 + 1;
      Gpr_Parser_Support.Internal.Introspection'Elab_Body;
      E695 := E695 + 1;
      E738 := E738 + 1;
      Gpr_Parser.Parsers'Elab_Spec;
      Gpr_Parser.Implementation'Elab_Spec;
      E750 := E750 + 1;
      Gpr_Parser.Analysis'Elab_Spec;
      Gpr_Parser.Generic_Introspection'Elab_Spec;
      E736 := E736 + 1;
      E662 := E662 + 1;
      Gpr_Parser.Common'Elab_Body;
      E649 := E649 + 1;
      Gpr_Parser.Generic_Impl'Elab_Spec;
      Gpr_Parser.Implementation'Elab_Body;
      E657 := E657 + 1;
      Gpr_Parser.Parsers'Elab_Body;
      E740 := E740 + 1;
      Gpr_Parser.Public_Converters'Elab_Spec;
      Gpr_Parser.Public_Converters'Elab_Body;
      E748 := E748 + 1;
      Gpr_Parser.Analysis'Elab_Body;
      E645 := E645 + 1;
      E651 := E651 + 1;
      E653 := E653 + 1;
      Gpr_Parser.Generic_Introspection'Elab_Body;
      E655 := E655 + 1;
      E752 := E752 + 1;
      E486 := E486 + 1;
      Sax.Pointers'Elab_Spec;
      Sax.Pointers'Elab_Body;
      E490 := E490 + 1;
      E571 := E571 + 1;
      Schema'Elab_Spec;
      E550 := E550 + 1;
      Unicode.Ccs'Elab_Spec;
      E482 := E482 + 1;
      E506 := E506 + 1;
      E508 := E508 + 1;
      E513 := E513 + 1;
      E516 := E516 + 1;
      E518 := E518 + 1;
      E520 := E520 + 1;
      E525 := E525 + 1;
      Unicode.Ces'Elab_Spec;
      E478 := E478 + 1;
      Sax.Symbols'Elab_Spec;
      Sax.Symbols'Elab_Body;
      E488 := E488 + 1;
      E548 := E548 + 1;
      Sax.Exceptions'Elab_Spec;
      Sax.Exceptions'Elab_Body;
      E546 := E546 + 1;
      E480 := E480 + 1;
      E528 := E528 + 1;
      E530 := E530 + 1;
      E484 := E484 + 1;
      Sax.Models'Elab_Spec;
      E544 := E544 + 1;
      Sax.Attributes'Elab_Spec;
      Sax.Attributes'Elab_Body;
      E542 := E542 + 1;
      Sax.Utils'Elab_Spec;
      Sax.Utils'Elab_Body;
      E492 := E492 + 1;
      DOM.CORE'ELAB_SPEC;
      E465 := E465 + 1;
      Schema.Date_Time'Elab_Spec;
      E562 := E562 + 1;
      E564 := E564 + 1;
      Schema.Simple_Types'Elab_Spec;
      E560 := E560 + 1;
      E504 := E504 + 1;
      E500 := E500 + 1;
      E498 := E498 + 1;
      E554 := E554 + 1;
      E496 := E496 + 1;
      E494 := E494 + 1;
      Input_Sources'Elab_Spec;
      Input_Sources'Elab_Body;
      E534 := E534 + 1;
      Input_Sources.File'Elab_Spec;
      Input_Sources.File'Elab_Body;
      E536 := E536 + 1;
      Input_Sources.Strings'Elab_Spec;
      Input_Sources.Strings'Elab_Body;
      E538 := E538 + 1;
      Sax.Readers'Elab_Spec;
      Sax.Readers'Elab_Body;
      E540 := E540 + 1;
      Schema.Validators'Elab_Spec;
      Schema.Readers'Elab_Spec;
      Schema.Schema_Readers'Elab_Spec;
      Schema.Schema_Readers'Elab_Body;
      E558 := E558 + 1;
      Schema.Readers'Elab_Body;
      E556 := E556 + 1;
      E573 := E573 + 1;
      Schema.Validators'Elab_Body;
      E569 := E569 + 1;
      Schema.Dom_Readers'Elab_Spec;
      Schema.Dom_Readers'Elab_Body;
      E552 := E552 + 1;
      GPR2.BUILD.SOURCE_BASE'ELAB_SPEC;
      E612 := E612 + 1;
      GPR2.PROJECT.REGISTRY.ATTRIBUTE'ELAB_SPEC;
      GPR2.PROJECT.ATTR_VALUES'ELAB_SPEC;
      GPR2.PROJECT.ATTR_VALUES'ELAB_BODY;
      E583 := E583 + 1;
      GPR2.PROJECT.ATTRIBUTE'ELAB_SPEC;
      GPR2.PROJECT.ATTRIBUTE'ELAB_BODY;
      E581 := E581 + 1;
      GPR2.PROJECT.ATTRIBUTE.SET'ELAB_SPEC;
      GPR2.PROJECT.ATTRIBUTE.SET'ELAB_BODY;
      E761 := E761 + 1;
      GPR2.PROJECT.ATTRIBUTE_CACHE'ELAB_SPEC;
      GPR2.PROJECT.ATTRIBUTE_CACHE'ELAB_BODY;
      E798 := E798 + 1;
      GPR2.PROJECT.NAME_VALUES'ELAB_SPEC;
      GPR2.PROJECT.NAME_VALUES'ELAB_BODY;
      E774 := E774 + 1;
      GPR2.PROJECT.REGISTRY.ATTRIBUTE.DESCRIPTION'ELAB_SPEC;
      GPR2.PROJECT.REGISTRY.ATTRIBUTE.DESCRIPTION'ELAB_BODY;
      E857 := E857 + 1;
      GPR2.PROJECT.TYP'ELAB_SPEC;
      GPR2.PROJECT.TYP'ELAB_BODY;
      E778 := E778 + 1;
      GPR2.PROJECT.TYP.SET'ELAB_SPEC;
      E801 := E801 + 1;
      GPR2.PROJECT.VARIABLE'ELAB_SPEC;
      GPR2.PROJECT.VARIABLE'ELAB_BODY;
      E772 := E772 + 1;
      GPR2.PROJECT.VARIABLE.SET'ELAB_SPEC;
      E779 := E779 + 1;
      GPR2.PACK_INTERNAL'ELAB_SPEC;
      E770 := E770 + 1;
      GPR2.PROJECT.VIEW'ELAB_SPEC;
      GPR2.BUILD.ACTIONS'ELAB_SPEC;
      GPR2.BUILD.COMPILATION_UNIT'ELAB_SPEC;
      GPR2.BUILD.COMPILATION_UNIT.MAPS'ELAB_SPEC;
      E608 := E608 + 1;
      GPR2.BUILD.SOURCE'ELAB_SPEC;
      E596 := E596 + 1;
      GPR2.PROJECT.REGISTRY.ATTRIBUTE'ELAB_BODY;
      E585 := E585 + 1;
      E846 := E846 + 1;
      GPR2.PROJECT.VIEW.SET'ELAB_SPEC;
      E605 := E605 + 1;
      GPR2.BUILD.VIEW_TABLES'ELAB_SPEC;
      GPR2.BUILD.VIEW_DB'ELAB_SPEC;
      GPR2.BUILD.SOURCE.SETS'ELAB_SPEC;
      GPR2.BUILD.TREE_DB'ELAB_SPEC;
      GPR2.BUILD.ACTIONS'ELAB_BODY;
      E822 := E822 + 1;
      GPR2.BUILD.SOURCE.SETS'ELAB_BODY;
      E604 := E604 + 1;
      GPR2.BUILD.SOURCE_BASE'ELAB_BODY;
      E598 := E598 + 1;
      GPR2.BUILD.VIEW_DB'ELAB_BODY;
      E607 := E607 + 1;
      GPR2.PROJECT.VIEW.VECTOR'ELAB_SPEC;
      E816 := E816 + 1;
      GPR2.PROJECT_PARSER'ELAB_SPEC;
      GPR2.PROJECT.CONFIGURATION'ELAB_SPEC;
      GPR2.KB'ELAB_SPEC;
      GPR2.KB.COMPILER_ITERATOR'ELAB_SPEC;
      GPR2.KB.COMPILER_ITERATOR'ELAB_BODY;
      E456 := E456 + 1;
      GPR2.KB.EMBEDDED'ELAB_SPEC;
      GPR2.KB.EMBEDDED'ELAB_BODY;
      E532 := E532 + 1;
      GPR2.KB.PARSING'ELAB_BODY;
      E462 := E462 + 1;
      GPR2.KB'ELAB_BODY;
      E433 := E433 + 1;
      GPR2.OPTIONS'ELAB_SPEC;
      GPR2.OPTIONS'ELAB_BODY;
      E406 := E406 + 1;
      E811 := E811 + 1;
      GPR2.PROJECT_PARSER.REGISTRY'ELAB_BODY;
      E783 := E783 + 1;
      GPR2.PROJECT_PARSER.SET'ELAB_SPEC;
      E802 := E802 + 1;
      GPR2.VIEW_INTERNAL'ELAB_SPEC;
      GPR2.PROJECT.CONFIGURATION'ELAB_BODY;
      E579 := E579 + 1;
      GPR2.TREE_INTERNAL'ELAB_SPEC;
      GPR2.BUILD.COMPILATION_UNIT'ELAB_BODY;
      E591 := E591 + 1;
      GPR2.BUILD.TREE_DB'ELAB_BODY;
      E593 := E593 + 1;
      GPR2.BUILD.VIEW_TABLES'ELAB_BODY;
      E610 := E610 + 1;
      GPR2.PROJECT.TREE'ELAB_SPEC;
      GPR2.PROJECT.TREE'ELAB_BODY;
      E815 := E815 + 1;
      GPR2.PROJECT.VIEW'ELAB_BODY;
      E589 := E589 + 1;
      GPR2.PROJECT_PARSER'ELAB_BODY;
      E767 := E767 + 1;
      GPR2.TREE_INTERNAL.VIEW_BUILDER'ELAB_SPEC;
      GPR2.TREE_INTERNAL.VIEW_BUILDER'ELAB_BODY;
      E809 := E809 + 1;
      GPR2.TREE_INTERNAL'ELAB_BODY;
      E758 := E758 + 1;
      GPR2.VIEW_INTERNAL'ELAB_BODY;
      E792 := E792 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/jicquel/projects/gpr-next/testsuite/tests/ali_parser/imports/obj/test.o
   --   -L/home/jicquel/projects/gpr-next/testsuite/tests/ali_parser/imports/obj/
   --   -L/home/jicquel/projects/gpr-next/testsuite/tests/ali_parser/imports/obj/
   --   -L/home/jicquel/projects/gpr-next/.build/debug/lib-static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnatcoll-core-current/install/lib/gnatcoll_core.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnatcoll-core-current/install/lib/gnatcoll_projects.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/libgpr-current/install/lib/gpr/static/gpr/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnat/install/lib/xmlada/xmlada_dom.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnat/install/lib/xmlada/xmlada_unicode.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnat/install/lib/xmlada/xmlada_sax.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnat/install/lib/xmlada/xmlada_input.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnat/install/lib/xmlada/xmlada_schema.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnatcoll-bindings-py3/install/lib/gnatcoll_iconv.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnatcoll-bindings-py3/install/lib/gnatcoll_gmp.static/
   --   -L/home/jicquel/sandboxes/gpr-next/x86_64-linux/gnat/install/lib/gcc/x86_64-pc-linux-gnu/13.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lrt
   --   -lpthread
   --   -ldl
--  END Object file/option list   

end ada_main;

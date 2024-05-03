gprinspect -Pabstract_project/prj -r
gprinspect -Padd_dir_to_project_path_support/prj -aP ./add_dir_to_project_path_support/dir_added -r
gprinspect -Paggregate/prj -r
gprinspect -Paggregate_library/prj -r
gprls -P ./check-shared-lib-import/prj.gpr  --unchecked-shared-lib-imports
gprinspect -Pencapsulated_library/prj -r
gprinspect -Pextends_all/prj -r
gprinspect -Pimplicit_with_support/prj --implicit-with=test1 --implicit-with=test2 -r
gprinspect -Pimported_and_extended/prj -r
gprinspect -Pmulti_executable/prj -r
gprinspect -Pnon_standalone_library/prj -r
gprinspect -Prelocate_build_tree_root_dir_support/prj/subdir/prj --root-dir=./relocate_build_tree_root_dir_support --relocate-build-tree=./relocate_build_tree_root_dir_support/build --src-subdirs=src_subdirs -r
gprinspect -Prelocate_build_tree_support/prj --relocate-build-tree=./relocate_build_tree_support/build --src-subdirs=src_subdirs -r
gprinspect -Pscenarios/prj -XMODE=release -r
gprls --source-parser --closure -Psource_overidden/prj
gprinspect -Pstatic_pic_library/prj -r

gprls --source-parser --closure -Pabstract_project/prj
gprls --source-parser --closure -Padd_dir_to_project_path_support/prj -aP ./add_dir_to_project_path_support/dir_added
gprls --source-parser --closure -Paggregate/prj
gprls --source-parser --closure -Paggregate_library/prj
gprls --source-parser --closure -P ./check-shared-lib-import/prj.gpr  --unchecked-shared-lib-imports
gprls --source-parser --closure -Pencapsulated_library/prj
gprls --source-parser --closure -Pextends_all/prj
gprls --source-parser --closure -Pimplicit_with_support/prj --implicit-with=test1 --implicit-with=test2
gprls --source-parser --closure -Pimported_and_extended/prj
gprls --source-parser --closure -Pmulti_executable/prj
gprls --source-parser --closure -Pnon_standalone_library/prj
gprls --source-parser --closure -Prelocate_build_tree_root_dir_support/prj/subdir/prj --root-dir=./relocate_build_tree_root_dir_support --relocate-build-tree=./relocate_build_tree_root_dir_support/build --src-subdirs=src_subdirs
gprls --source-parser --closure -Prelocate_build_tree_support/prj --relocate-build-tree=./relocate_build_tree_support/build --src-subdirs=src_subdirs
gprls --source-parser --closure -Pscenarios/prj -XMODE=release
gprls --source-parser --closure -Psource_overidden/prj
gprls --source-parser --closure -Pstatic_pic_library/prj
gprls --source-parser --closure -Psource_overidden/prj
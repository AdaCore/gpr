with Ada.Text_IO;
with Ada.Command_Line;

with GPR2.Options;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Exchange;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Pack.Description;

procedure check_prove is
   use GPR2;

   package PRP renames GPR2.Project.Registry.Pack;
   package PRPD renames GPR2.Project.Registry.Pack.Description;
   package PRA renames GPR2.Project.Registry.Attribute;
   package PRAD renames GPR2.Project.Registry.Attribute.Description;

   Check_Package          : constant Package_Id := +"Check";
   Check_Default_Switches : constant Q_Attribute_Id :=
                              (Check_Package, +"Default_Switches");
   Check_Switches         : constant Q_Attribute_Id :=
                              (Check_Package, +"Switches");

   Codepeer_Package       : constant Package_Id := +"Codepeer";
   Codepeer_File_Patterns : constant Q_Attribute_Id :=
                              (Codepeer_Package, +"File_Patterns");

   Prove_Package        : constant Package_Id := +"Prove";
   Prove_Switches       : constant Q_Attribute_Id :=
                            (Prove_Package, +"Switches");
   Prove_Proof_Switches : constant Q_Attribute_Id :=
                            (Prove_Package, +"Proof_Switches");
   Prove_Proof_Dir      : constant Q_Attribute_Id :=
                            (Prove_Package, +"Proof_Dir");

   Options : GPR2.Options.Object;
begin

   if not PRP.Exists (Check_Package) then
      PRP.Add
        (Name     => Check_Package,
         Projects => PRP.Everywhere);
   end if;

   if PRPD.Get_Package_Description (Check_Package) = "" then
      PRPD.Set_Package_Description
        (Key         => Check_Package,
         Description => "This package specifies the options used when " &
           "calling the checking tool gnatcheck. Its attribute " &
           "Default_Switches has the same semantics as for the package " &
           "Builder. The first string should always be -rules to specify " &
           "that all the other options belong to the -rules section of " &
           "the parameters to gnatcheck.");
   end if;

   if not PRA.Exists (Check_Default_Switches) then
      PRA.Add
        (Name                  => Check_Default_Switches,
         Index_Type            => PRA.Language_Index,
         Value                 => PRA.List,
         Value_Case_Sensitive  => True,
         Is_Allowed_In         => PRA.Everywhere,
         Config_Concatenable   => True);
   end if;

   if PRAD.Get_Attribute_Description (Check_Default_Switches) = "" then
      PRAD.Set_Attribute_Description
        (Key         => Check_Default_Switches,
         Description => "Index is a language name. Value is a list of " &
           "switches to be used when invoking gnatcheck for a source of " &
           "the language, if there is no applicable attribute Switches.");
   end if;

   if not PRA.Exists (Check_Switches) then
      PRA.Add
        (Name                  => Check_Switches,
         Index_Type            => PRA.File_Index,
         Value                 => PRA.List,
         Value_Case_Sensitive  => True,
         Is_Allowed_In         => PRA.Everywhere,
         Index_Optional        => True,
         Config_Concatenable   => True);
   end if;

   if PRAD.Get_Attribute_Description (Check_Switches) = "" then
      PRAD.Set_Attribute_Description
        (Key         => Check_Switches,
         Description => "Index is a source file name. Value is the list " &
           "of switches to be used when invoking gnatcheck for the " &
           "source.");
   end if;

   if not PRP.Exists (Codepeer_Package) then
      PRP.Add
        (Name     => Codepeer_Package,
         Projects => PRP.Everywhere);
   end if;

   if PRPD.Get_Package_Description (Codepeer_Package) = "" then
      PRPD.Set_Package_Description
        (Key         => Codepeer_Package,
         Description => "This package specifies the options used when " &
           "calling codepeer or calling gnatcheck with --simple-project " &
           "switch. Default_Switches has the same semantics as for the " &
           "package Builder. The first string should always be -rules to " &
           "specify that all the other options belong to the -rules " &
           "section of the parameters to gnatcheck.");
   end if;

   if not PRA.Exists (Codepeer_File_Patterns) then
      PRA.Add
        (Name                  => Codepeer_File_Patterns,
         Index_Type            => PRA.No_Index,
         Value                 => PRA.List,
         Value_Case_Sensitive  => True,
         Is_Allowed_In         => PRA.Everywhere);
   end if;

   if PRAD.Get_Attribute_Description (Codepeer_File_Patterns) = "" then
      PRAD.Set_Attribute_Description
        (Key         => Codepeer_File_Patterns,
         Description => "If you want to override ada default file " &
           "extensions (ada, ads, adb, spc & bdy), use this attribute " &
           "which includes a list of file patterns where you can specify " &
           "the following meta characters: * : matches any string of 0 " &
           "or more characters, ? : matches any character, " &
           " [list of chars] : matches any character listed, [char-char] " &
           ": matches any character in given range, [^list of chars] : " &
           "matches any character not listed. These patterns are case " &
           "insensitive.");
   end if;

   if not PRP.Exists (Prove_Package) then
      PRP.Add
        (Name     => Prove_Package,
         Projects => PRP.Everywhere);
   end if;

   if PRPD.Get_Package_Description (Prove_Package) = "" then
      PRPD.Set_Package_Description
        (Key         => Prove_Package,
         Description => "This package specifies the options used when " &
           "calling gnatprove");
   end if;

   if not PRA.Exists (Prove_Proof_Switches) then
      PRA.Add
        (Name                  => Prove_Proof_Switches,
         Index_Type            => PRA.FileGlob_Or_Language_Index,
         Value                 => PRA.List,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         => PRA.Everywhere);
   end if;

   if PRAD.Get_Attribute_Description (Prove_Proof_Switches) = "" then
      PRAD.Set_Attribute_Description
        (Key         => Prove_Proof_Switches,
         Description => "Defines additional command line switches that " &
           "are used for the invokation of GNATprove. Only the following " &
           "switches are allowed for file-specific switches: --steps, " &
           "--timeout, --memlimit, --proof, --prover, --level, --mode, " &
           "--counterexamples, --no-inlining, --no-loop-unrolling");
   end if;

   if not PRA.Exists (Prove_Switches) then
      PRA.Add
        (Name                  => Prove_Switches,
         Index_Type            => PRA.No_Index,
         Value                 => PRA.List,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         => PRA.Everywhere);
   end if;

   if PRAD.Get_Attribute_Description (Prove_Switches) = "" then
      PRAD.Set_Attribute_Description
        (Key         => Prove_Switches,
         Description => "This deprecated attribute is the same as " &
           "Proof_Switches (""Ada"").");
   end if;

   if not PRA.Exists (Prove_Proof_Dir) then
      PRA.Add
        (Name                  => Prove_Proof_Dir,
         Index_Type            => PRA.No_Index,
         Value                 => PRA.Single,
         Value_Case_Sensitive  => True,
         Is_Allowed_In         => PRA.Everywhere);
   end if;

   if PRAD.Get_Attribute_Description (Prove_Proof_Dir) = "" then
      PRAD.Set_Attribute_Description
        (Key         => Prove_Proof_Dir,
         Description => "Defines the directory where are stored the " &
           "files concerning the state of the proof of a project. This " &
           "directory contains a sub-directory sessions with one " &
           "directory per source package analyzed for proof. Each of " &
           "these package directories contains a Why3 session file. If a " &
           "manual prover is used to prove some VCs, then a " &
           "sub-directory called by the name of the prover is created " &
           "next to sessions, with the same organization of " &
           "sub-directories. Each of these package directories contains " &
           "manual proof files. Common proof files to be used across " &
           "various proofs can be stored at the toplevel of the " &
           "prover-specific directory.");
   end if;

   if Ada.Command_Line.Argument_Count > 0 and then
     Ada.Command_Line.Argument (1) = "--help"
   then
      Ada.Text_IO.Put_Line
        (Item => "Usage:" & ASCII.LF &
           "  check_prove [opts]" & ASCII.LF &
           ASCII.LF &
           "  --help" & ASCII.LF &
           "    Display usage and exit" & ASCII.LF &
           ASCII.LF &
           "  " & GPR2.Options.Print_GPR_Registry_Option & ASCII.LF &
           "    Display registered packages/attributes and exit" & ASCII.LF);
      return;
   end if;

   Options.Add_Switch (GPR2.Options.No_Project);
   Options.Add_Switch (GPR2.Options.Print_GPR_Registry);
   Options.Print_GPR_Registry
     (Format => GPR2.Project.Registry.Exchange.K_JSON);

end check_prove;

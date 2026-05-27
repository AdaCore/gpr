with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Containers;
with GPR2.Options;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Exchange;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Pack.Description;

with Test;

procedure export is
   use GPR2;

   package PRP renames GPR2.Project.Registry.Pack;
   package PRPD renames GPR2.Project.Registry.Pack.Description;
   package PRA renames GPR2.Project.Registry.Attribute;
   package PRAD renames GPR2.Project.Registry.Attribute.Description;
   package PRE renames GPR2.Project.Registry.Exchange;

   Package_For_Aggregate     : constant Package_Id := +"Package_For_Aggregate";
   Package_For_Non_Aggregate : constant Package_Id :=
                                 +"Package_For_Non_Aggregate";
   Values : PRA.Value_Map.Map;

   Type_Def : PRA.Attribute_Type;

   Options : GPR2.Options.Object;

   procedure Test_Attribute
     (Package_Name          : Package_Id;
      Attribute_Name        : Optional_Attribute_Id;
      Comment               : String;
      Index_Type            : PRA.Index_Value_Type           := PRA.No_Index;
      Value                 : PRA.Value_Kind                 := PRA.List;
      Value_Case_Sensitive  : Boolean                        := False;
      Is_Allowed_In         : PRA.Allowed_In                 := PRA.Everywhere;
      Type_Def              : PRA.Attribute_Type             := PRA.No_Attribute_Type;
      Is_Builtin            : Boolean                        := False;
      Index_Optional        : Boolean                        := False;
      Empty_Value           : PRA.Empty_Value_Status         := PRA.Allow;
      Default               : PRA.Default_Value              := PRA.No_Default_Value;
      Has_Default_In        : PRA.Allowed_In                 := PRA.Nowhere;
      Is_Toolchain_Config   : Boolean                        := False;
      Config_Concatenable   : Boolean                        := False;
      Inherit_From_Extended : PRA.Inherit_From_Extended_Type := PRA.Inherited;
      Is_Set                : Boolean                        := False) is
      Name : constant GPR2.Q_Attribute_Id := (Package_Name, Attribute_Name);
   begin
      PRA.Add
        (Name                  => Name,
         Index_Type            => Index_Type,
         Value                 => Value,
         Value_Case_Sensitive  => Value_Case_Sensitive,
         Is_Allowed_In         => Is_Allowed_In,
         Type_Def              => Type_Def,
         Is_Builtin            => Is_Builtin,
         Index_Optional        => Index_Optional,
         Empty_Value           => Empty_Value,
         Default               => Default,
         Has_Default_In        => Has_Default_In,
         Is_Toolchain_Config   => Is_Toolchain_Config,
         Config_Concatenable   => Config_Concatenable,
         Inherit_From_Extended => Inherit_From_Extended,
         Is_Set                => Is_Set);
      PRAD.Set_Attribute_Description
        (Key         => Name,
         Description => Comment);
   end Test_Attribute;

begin
   Values.Insert ("Ada", "Ada_Value");
   Values.Insert ("C", "C_Value");

   Type_Def.Include ("true");
   Type_Def.Include ("false");

   PRP.Add
     (Name     => Package_For_Aggregate,
      Projects => (Aggregate_Kind => True, others => False));
   PRPD.Set_Package_Description
     (Key         => Package_For_Aggregate,
      Description => "test package description and projects aggregate");

   PRP.Add
     (Name     => Package_For_Non_Aggregate,
      Projects => (Aggregate_Kind => False, others => True));
   PRPD.Set_Package_Description
     (Key         => Package_For_Non_Aggregate,
      Description => "test projects non aggregate");

   Test_Attribute
     (Package_Name          => Package_For_Aggregate,
      Attribute_Name        => +"Attribute_1",
      Comment               => "Test No_Index, Single," &
        " not Value_Case_Sensitive," &
        " Is_Allowed_In aggregate, Type_Def, not Is_Builtin," &
        " not Index_Optional, Allow Empty_Value, D_Callback default," &
        " Has_Default_In aggregate, not Is_Toolchain_Config," &
        " not Config_Concatenable, not Is_Set",
      Index_Type            => PRA.No_Index,
      Value                 => PRA.Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => (Aggregate_Kind => True, others => False),
      Type_Def              => Type_Def,
      Is_Builtin            => False,
      Index_Optional        => False,
      Empty_Value           => PRA.Allow,
      Default               => (Kind => PRA.D_Callback, Callback => Test.DLSA),
      Has_Default_In        => (Aggregate_Kind => True, others => False),
      Is_Toolchain_Config   => False,
      Config_Concatenable   => False,
      Is_Set                => False);

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_2",
      Comment               => "Test String_Index, List," &
        " Value_Case_Sensitive," &
        " Is_Allowed_In not aggregate, No_Attribute_Type, Is_Builtin," &
        " Index_Optional, Ignore Empty_Value, No_Default_Value," &
        " Has_Default_In Nowhere, Is_Toolchain_Config," &
        " Config_Concatenable, Is_Set",
      Index_Type            => PRA.String_Index,
      Value                 => PRA.List,
      Value_Case_Sensitive  => True,
      Is_Allowed_In         => (Aggregate_Kind => False, others => True),
      Type_Def              => PRA.No_Attribute_Type,
      Is_Builtin            => True,
      Index_Optional        => True,
      Empty_Value           => PRA.Ignore,
      Default               => PRA.No_Default_Value,
      Has_Default_In        => PRA.Nowhere,
      Is_Toolchain_Config   => True,
      Config_Concatenable   => True,
      Is_Set                => True);

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_3",
      Comment               => "Test Language_Index, Single," &
        " Is_Allowed_In non aggregate, Empty_Value Error," &
        " D_Value Default, Has_Default_In non aggregate,",
      Index_Type            => PRA.Language_Index,
      Value                 => PRA.Single,
      Is_Allowed_In         => (Aggregate_Kind => False, others => True),
      Empty_Value           => PRA.Error,
      Default               => (Kind => PRA.D_Value, Values => Values),
      Has_Default_In        => (Aggregate_Kind => False, others => True));

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_4",
      Comment               => "Test Language_Index, Single," &
        " Is_Allowed_In non aggregate, Empty_Value Error," &
        " D_Attribute_Reference Default, Has_Default_In non aggregate,",
      Index_Type            => PRA.Language_Index,
      Value                 => PRA.Single,
      Is_Allowed_In         => (Aggregate_Kind => False, others => True),
      Empty_Value           => PRA.Error,
      Default               => (Kind => PRA.D_Attribute_Reference,
                                Attr => +"Attribute_3"),
      Has_Default_In        => (Aggregate_Kind => False, others => True));

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_5",
      Comment               => "Test Unit_Index",
      Index_Type            => PRA.Unit_Index);

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_6",
      Comment               => "Test Env_Var_Name_Index",
      Index_Type            => PRA.Env_Var_Name_Index);

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_7",
      Comment               => "Test File_Index",
      Index_Type            => PRA.File_Index);

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_8",
      Comment               => "Test FileGlob_Index",
      Index_Type            => PRA.FileGlob_Index);

   Test_Attribute
     (Package_Name          => Package_For_Non_Aggregate,
      Attribute_Name        => +"Attribute_9",
      Comment               => "Test FileGlob_Or_Language_Index",
      Index_Type            => PRA.FileGlob_Or_Language_Index);

   for Inherited_Mode in PRA.Inherit_From_Extended_Type loop
      Test_Attribute
        (Package_Name          => GPR2.Project_Level_Scope,
         Attribute_Name        => +GPR2.Optional_Name_Type
                                     ("Attribute_" & Inherited_Mode'Img),
         Comment               => "",
         Inherit_From_Extended => Inherited_Mode
        );
   end loop;

   if Ada.Command_Line.Argument_Count > 0 then
      if Ada.Command_Line.Argument (1) = "--help" then
         Ada.Text_IO.Put_Line
           (Item => "Usage:" & ASCII.LF &
              "  export [opts]" & ASCII.LF &
              ASCII.LF &
              "  --help" & ASCII.LF &
              "    Display usage and exit" & ASCII.LF &
              ASCII.LF &
              "  " & GPR2.Options.Print_GPR_Registry_Option & ASCII.LF &
              "    Display registered packages/attributes and exit" & ASCII.LF);
      elsif Ada.Command_Line.Argument (1) =
        GPR2.Options.Print_GPR_Registry_Option
      then
         Options.Add_Switch (GPR2.Options.Print_GPR_Registry);
         Options.Add_Switch (GPR2.Options.No_Project);
         Options.Print_GPR_Registry (Format => PRE.K_JSON);
      end if;
   else
      declare
         Project_Level : GPR2.Containers.Package_Id_List;
      begin
         Project_Level.Insert (GPR2.Project_Level_Scope);

         GPR2.Project.Registry.Exchange.Export
           (Included => Project_Level,
            Excluded => GPR2.Containers.Package_Id_Type_List.Empty_Set);
      end;
   end if;

end export;

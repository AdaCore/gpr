with Ada.Text_IO;
with GPR2.Message;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute.Set;
with GPR2.Source_Reference.Value;
procedure main is
   use GPR2;
   use type GPR2.Message.Level_Value;

   Ignore_Single : constant GPR2.Q_Attribute_Id :=
                      (GPR2.Project_Level_Scope, +"ignore_single");
   Error_Single   : constant GPR2.Q_Attribute_Id :=
                      (GPR2.Project_Level_Scope, +"error_single");
   Allow_Single   : constant GPR2.Q_Attribute_Id :=
                      (GPR2.Project_Level_Scope, +"allow_single");
   Ignore_List   : constant GPR2.Q_Attribute_Id :=
                      (GPR2.Project_Level_Scope, +"ignore_list");
   Error_List     : constant GPR2.Q_Attribute_Id :=
                      (GPR2.Project_Level_Scope, +"error_list");
   Allow_List     : constant GPR2.Q_Attribute_Id :=
                      (GPR2.Project_Level_Scope, +"allow_list");

   procedure Test (Name : String) is
      Tree : GPR2.Project.Tree.Object;
      Opt  : Options.Object;

      procedure Print_Attributes (Name : Q_Attribute_Id) is
         Attributes : GPR2.Project.Attribute.Set.Object;
         Header     : constant String :=
                        (if Name.Pack = Project_Level_Scope
                         then Image (Name.Attr)
                         else Image (Name.Pack)
                         & "." & Image (Name.Attr));
      begin
         Attributes := Tree.Root_Project.Attributes
           (Name          => Name,
            With_Defaults => False,
            With_Config   => False);

         for A of Attributes loop
            declare
               Attribute : constant GPR2.Project.Attribute.Object := A;
               use GPR2.Project.Registry.Attribute;
            begin
               Ada.Text_IO.Put (Header);
               if Attribute.Has_Index then
                  Ada.Text_IO.Put ("(" & Attribute.Index.Text & ")");
               end if;
               Ada.Text_IO.Put ("=");
               if Attribute.Kind = GPR2.Project.Registry.Attribute.Single then
                  Ada.Text_IO.Put ("""");
                  Ada.Text_IO.Put (String (Attribute.Value.Text));
                  Ada.Text_IO.Put ("""");
               else
                  declare
                     Separator : Boolean := False;
                     Value     : GPR2.Source_Reference.Value.Object;
                  begin
                     Ada.Text_IO.Put ("(");
                     for V of Attribute.Values loop
                        Value := V;
                        if Separator then
                           Ada.Text_IO.Put (",");
                        end if;
                        Separator := True;
                        Ada.Text_IO.Put ("""");
                        Ada.Text_IO.Put (String (Value.Text));
                        Ada.Text_IO.Put ("""");
                     end loop;
                     Ada.Text_IO.Put (")");
                  end;
               end if;
               Ada.Text_IO.Put_Line ("");
            end;

         end loop;
      end Print_Attributes;
   begin
      Opt.Add_Switch (Options.P, Name);
      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Print_Attributes (Allow_Single);
         Print_Attributes (Allow_List);
         Print_Attributes (Ignore_Single);
         Print_Attributes (Ignore_List);

      else
         --  Also print the warnings
         for Log of Tree.Log_Messages.all loop
            if Log.Level = GPR2.Message.Warning then
               Ada.Text_IO.Put_Line (Log.Format);
            end if;
         end loop;
      end if;
   end Test;

   Default_Value : GPR2.Project.Registry.Attribute.Default_Value
     (GPR2.Project.Registry.Attribute.D_Value);

begin
   Default_Value.Values.Insert
     (GPR2.Project.Registry.Attribute.Any_Index, "default");

   GPR2.Project.Registry.Attribute.Add
     (Name                  => Allow_Single,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere,
      Empty_Value           => GPR2.Project.Registry.Attribute.Allow);
   GPR2.Project.Registry.Attribute.Add
     (Name                  => Allow_List,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.List,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere,
      Empty_Value           => GPR2.Project.Registry.Attribute.Allow);
   GPR2.Project.Registry.Attribute.Add
     (Name                  => Ignore_Single,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere,
      Empty_Value           => GPR2.Project.Registry.Attribute.Ignore);
   GPR2.Project.Registry.Attribute.Add
     (Name                  => Ignore_List,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.List,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere,
      Empty_Value           => GPR2.Project.Registry.Attribute.Ignore);
   GPR2.Project.Registry.Attribute.Add
     (Name                  => Error_Single,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.Single,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere,
      Empty_Value           => GPR2.Project.Registry.Attribute.Error);
   GPR2.Project.Registry.Attribute.Add
     (Name                  => Error_List,
      Index_Type            => GPR2.Project.Registry.Attribute.No_Index,
      Value                 => GPR2.Project.Registry.Attribute.List,
      Value_Case_Sensitive  => False,
      Is_Allowed_In         => GPR2.Project.Registry.Attribute.Everywhere,
      Empty_Value           => GPR2.Project.Registry.Attribute.Error);
   Test ("warning.gpr");
   Test ("error.gpr");
end main;

with Ada.Directories;
with Ada.Text_IO;

with GPR2.Build.View_Db;
with GPR2.Log;
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;

with Test_Assert;

function Test return Integer is
   use GPR2;
   use type Message.Level_Value;
   Tree : Project.Tree.Object;
   Opt  : Options.Object;

   procedure Check_Message
     (Expected     : String;
      Project_File : Simple_Name := "root.gpr";
      Level        : Message.Level_Value := Message.Warning);
   procedure Write_File (Name, Content : String);
   procedure Write_List (Name, Content : String);

   procedure Check_Message
     (Expected     : String;
      Project_File : Simple_Name := "root.gpr";
      Level        : Message.Level_Value := Message.Warning)
   is
      Messages : Log.Object;
      Found    : Boolean := False;
   begin
      Tree.Root_Project.View_Db.Update (Messages);
      Test_Assert.Assert
        (Messages.Has_Error = (Level in Message.Any_Error));
      for M of Messages loop
         if M.Message = Expected then
            Found := True;
            Test_Assert.Assert (M.Level = Level);
            Test_Assert.Assert
              (Path_Name.Simple_Name (M.Sloc.Filename) = Project_File);
         end if;
      end loop;
      Test_Assert.Assert (Found);
   end Check_Message;

   procedure Write_File (Name, Content : String) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, "tree/" & Name);
      Ada.Text_IO.Put (File, Content);
      Ada.Text_IO.Close (File);
   end Write_File;

   procedure Write_List (Name, Content : String) is
   begin
      Write_File (Name & ".lst", Content);
   end Write_List;

begin
   Opt.Add_Switch (Options.P, "tree/root.gpr");
   Test_Assert.Assert (Tree.Load (Opt, Absent_Dir_Error => No_Error));
   Test_Assert.Assert (Tree.Update_Sources (Sources_Only));

   declare
      Db : constant Build.View_Db.Object := Tree.Root_Project.View_Db;
   begin
      Test_Assert.Assert (Db.Has_Source ("kept.ads"));
      Test_Assert.Assert (not Db.Has_Source ("changed.ads"));
      Test_Assert.Assert (not Db.Has_Source ("inherited.ads"));
      Test_Assert.Assert
        (Db.Excluded_Sources.Contains
           (Path_Name.Create_File ("tree/own/changed.ads")));
      Test_Assert.Assert (not Db.Excluded_Inherited_Sources.Is_Empty);

      Write_List ("included", "");
      Write_List ("excluded", "");
      Test_Assert.Assert (Tree.Update_Sources (Sources_Only));
      Test_Assert.Assert (not Db.Has_Source ("kept.ads"));
      Test_Assert.Assert (not Db.Has_Source ("changed.ads"));
      Test_Assert.Assert (Db.Has_Source ("inherited.ads"));

      Write_List ("included", "changed.ads" & ASCII.LF & "inherited.ads");

      for Attempt in 1 .. 2 loop
         Test_Assert.Assert (Tree.Update_Sources (Sources_Only));
         Test_Assert.Assert (not Db.Has_Source ("kept.ads"));
         Test_Assert.Assert (Db.Has_Source ("changed.ads"));
         Test_Assert.Assert (Db.Has_Source ("inherited.ads"));
         Test_Assert.Assert (Db.Excluded_Sources.Is_Empty);
         Test_Assert.Assert (Db.Excluded_Inherited_Sources.Is_Empty);
      end loop;

      Write_List ("excluded", "missing.ads");
      Check_Message ("unknown excluded file ""missing.ads""");
      Write_List ("excluded", "");
      Test_Assert.Assert (Tree.Update_Sources (Sources_Only));

      Write_List ("included", "missing.ads");
      Check_Message
        ("source file ""missing.ads"" not found",
         Level => Message.Critical_Error);
      Write_List ("included", "changed.ads" & ASCII.LF & "inherited.ads");
      Test_Assert.Assert (Tree.Update_Sources (Sources_Only));
      Test_Assert.Assert (Db.Has_Source ("changed.ads"));
      Test_Assert.Assert (Db.Has_Source ("inherited.ads"));

      Write_File ("own/inherited.ads", "package Inherited is end Inherited;");
      Ada.Directories.Delete_File ("tree/base/inherited.ads");
      Check_Message
        ("source file ""inherited.ads"" not found", "base.gpr",
         Message.Critical_Error);
      Test_Assert.Assert (Db.Has_Source ("inherited.ads"));
      Write_File ("base/inherited.ads", "package Inherited is end Inherited;");
      Test_Assert.Assert (Tree.Update_Sources (Sources_Only));
   end;

   return Test_Assert.Report;
end Test;

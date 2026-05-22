with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.OS.FS;
with GNATCOLL.Utils;

with GPR2.Build.Command_Line;
with GPR2.Build.Response_Files;
with GPR2.Containers;

with GPR2.Source_Reference.Value;
with Test_Helper;

function Main return Integer is

   use GPR2;
   use GPR2.Build;
   use type Response_Files.Response_File_Kind;

   procedure Run_Test_Case
     (Format       : Response_Files.Response_File_Format;
      Kind         : Response_Files.Response_File_Kind;
      Max_Length   : Natural;
      Switches     : Containers.Source_Value_List;
      Encapsulated : Boolean := False);

   procedure Run_Test_Case
     (Format       : Response_Files.Response_File_Format;
      Kind         : Response_Files.Response_File_Kind;
      Max_Length   : Natural;
      Switches     : Containers.Source_Value_List;
      Encapsulated : Boolean := False)
   is
      RF       : Response_Files.Object;
      Cmd_Line : Command_Line.Object;
   begin
      Test_Helper.New_Test_Case
        ("Response File : " & Format'Img
         & " - " & Kind'Img & " -" & Max_Length'Img
         & ((if Encapsulated then " - Encapsulated" else "")));

      Cmd_Line.Set_Driver ("random_driver");
      Cmd_Line.Add_Argument ("-a");
      Cmd_Line.Add_Argument ("-b");
      Cmd_Line.Add_Argument ("foo.o", Kind => Command_Line.Obj);
      Cmd_Line.Add_Argument ("bar.o", Kind => Command_Line.Obj);
      Cmd_Line.Add_Argument ("-c");
      Cmd_Line.Add_Argument ("-d");
      Cmd_Line.Add_Argument ("check format");

      Ada.Text_IO.Put_Line (Test_Helper.Image (Cmd_Line.Argument_List));

      if Cmd_Line.Total_Length > Max_Length then
         RF.Initialize
           (Format     => Format,
            Kind       => Kind,
            Switches   => Switches);

         declare
            Primary_Name   : constant String :=
                               GNATCOLL.Utils.Executable_Location & "primary";
            Secondary_Name : constant String :=
                               GNATCOLL.Utils.Executable_Location
                               & "secondary";
            Primary_FD     : constant GNATCOLL.OS.FS.File_Descriptor :=
                               GNATCOLL.OS.FS.Open
                                 (Primary_Name, GNATCOLL.OS.FS.Write_Mode);

            Secondary_FD   : constant GNATCOLL.OS.FS.File_Descriptor :=
                               (if Encapsulated
                                then GNATCOLL.OS.FS.Open
                                  (Secondary_Name, GNATCOLL.OS.FS.Write_Mode)
                                else GNATCOLL.OS.FS.Invalid_FD);
         begin
            if Encapsulated then
               RF.Register (Secondary_FD, Filename_Type (Secondary_Name), True);
            end if;
            RF.Register (Primary_FD, Filename_Type (Primary_Name));

            RF.Create (Cmd_Line);

            Ada.Text_IO.Put_Line (Test_Helper.Image (Cmd_Line.Argument_List));
            if RF.Has_Secondary_Content then
               Ada.Text_IO.Put_Line
                 (Test_Helper.Image_RF
                    (RF.Secondary_Response_File,
                     RF.Secondary_Response_File_Content));
            end if;
            Ada.Text_IO.Put_Line
              (Test_Helper.Image_RF
                 (RF.Primary_Response_File,
                  RF.Primary_Response_File_Content));
         end;
      else
         Ada.Text_IO.Put_Line (Test_Helper.Image (Cmd_Line.Argument_List));
      end if;
   end Run_Test_Case;

   RFS      : Containers.Source_Value_List;
   First    : constant Source_Reference.Value.Object'Class :=
                Source_Reference.Value.Create ("foo/bar", 1, 1, "-rfswitch");
   Second   : constant Source_Reference.Value.Object'Class :=
                Source_Reference.Value.Create ("foo/bar", 1, 1, "<at>");
begin
   RFS.Append (Source_Reference.Value.Object (First));
   RFS.Append (Source_Reference.Value.Object (Second));

   for K in Response_Files.Response_File_Kind loop
      if K = Response_Files.Compiler or else K = Response_Files.Binder then
         Ada.Text_IO.Put_Line
           ("======================================================"
            & "==================================");
         Ada.Text_IO.Put_Line
           ("===== No differenciation between response file format for "
            & "compilation command line =====");
         Ada.Text_IO.Put_Line
           ("======================================================"
            & "==================================");
      elsif K = Response_Files.Unknown then
         Ada.Text_IO.Put_Line
           ("======================================================"
            & "===================================");
         Ada.Text_IO.Put_Line
           ("===== Unknown kind, no response files should be used, "
            & "raw command line used instead =====");
         Ada.Text_IO.Put_Line
           ("======================================================"
            & "===================================");
      end if;

      for F in Response_Files.Response_File_Format loop
         Run_Test_Case (F, K, 1000, RFS);
         Run_Test_Case (F, K, 1, RFS);
         Run_Test_Case (F, K, 1000, RFS, True);
         Run_Test_Case (F, K, 1, RFS, True);
      end loop;
   end loop;

   return Test_Helper.Result;

end Main;

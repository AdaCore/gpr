
--  Main application using a Fortran and C routine

with Ada.Numerics.Real_Arrays;
with Ada.Float_Text_IO;
with Ada.Text_IO;

with Interfaces.C;
with Interfaces.Fortran;

procedure Main is

   use Ada;
   use Ada.Numerics.Real_Arrays;
   use Interfaces;
   use type Fortran.Logical;

   type Matrix is array (Integer range <>, Integer range <>) of Float'Base;
   --  Root matrix type, derived below with C and Fortran convention

   --  C binding

   type C_Matrix is new Matrix (1 .. 3, 1 .. 3);
   pragma Convention (C, C_Matrix);

   procedure initmat (M : in out C_Matrix; Line, Column : C.int);
   pragma Import (C, initmat, "initmat");

   --  Fortran binding

   type F_Matrix is new Matrix (1 .. 3, 1 .. 3);
   pragma Convention (Fortran, F_Matrix);

   procedure multmat
     (Res :    out Fortran.Logical;
      M1  : in     F_Matrix; Line1, Column1 : Fortran.Fortran_Integer;
      M2  : in     F_Matrix; Line2, Column2 : Fortran.Fortran_Integer;
      M3  :    out F_Matrix);
   pragma Import (Fortran, Multmat);
   pragma Import_Valued_Procedure (Multmat);

   --  Ada code

   procedure Copy (FM : in F_Matrix; AM : out Real_Matrix) is
   begin
      --  We should check the ranges here
      for L in FM'Range (1) loop
         for C in FM'Range (2) loop
            AM (L, C) := FM (L, C);
         end loop;
      end loop;
   end Copy;

   procedure Display (M : in Real_Matrix) is
   begin
      for L in M'Range (1) loop
         Text_IO.Put ("| ");
         for C in M'Range (2) loop
            Float_Text_IO.Put (M (L , C), Fore => 3, Aft => 4, Exp => 0);
            Text_IO.Put (", ");
         end loop;
         Text_IO.Put_Line (" |");
      end loop;
      Text_IO.New_Line;
   end Display;

   CM1, CM2      : C_Matrix;
   FM1, FM2, FM3 : F_Matrix;
   Res           : Fortran.Logical;
   AM1, AM2      : Real_Matrix (1 .. 3, 1 .. 3);
   D             : Float;

begin
   --  Initialize matrix with the C routine

   initmat (CM1, C.int (CM1'Length (1)), C.int (CM1'Length (2)));
   initmat (CM2, C.int (CM2'Length (1)), C.int (CM2'Length (2)));

   --  Multiply both matrix using the Fortran routine
   --  The following copies encure the convertion between both convention
   --  (line, column order being different in C/Ada and Fortran.

   FM1 := F_Matrix (CM1);
   FM2 := F_Matrix (CM2);

   multmat (Res,
            FM1,
            Fortran.Fortran_Integer (FM1'Length (1)),
            Fortran.Fortran_Integer (FM1'Length (2)),
            FM2,
            Fortran.Fortran_Integer (FM2'Length (1)),
            Fortran.Fortran_Integer (FM2'Length (2)),
            FM3);

   if not Res then
      Text_IO.Put_Line ("Dimentions are not compatible");
      return;
   end if;

   --  Transpose the matrix using Ada.Numerics support.
   --  We need to copy the Fortran matrix into the Ada one.

   Copy (FM3, AM1);

   AM2 := AM1 / 2.0;

   Text_IO.Put_Line ("Matrix :");
   Display (AM1);

   Text_IO.Put_Line ("Matrix divided by 2 :");
   Display (AM2);

   D := Determinant (AM2);
   Text_IO.Put ("Corresponding determinant :");
   Float_Text_IO.Put (D, Fore => 3, Aft => 7, Exp => 0);
   Text_IO.New_Line;
end Main;

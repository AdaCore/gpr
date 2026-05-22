with Text_Io; use Text_Io;
with Db;
package body Mmi is
  procedure Open is
  begin
    Put_Line ("open mmi (user)");
    Db.Open;
  end Open;
end Mmi;

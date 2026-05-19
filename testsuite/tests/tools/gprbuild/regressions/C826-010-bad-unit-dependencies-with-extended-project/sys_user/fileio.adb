with Text_Io; use Text_Io;
with Db;
package body Fileio is
  procedure Open is
  begin
    Db.Open;
    Put_Line ("open sys (user)");
  end Open;
end Fileio;

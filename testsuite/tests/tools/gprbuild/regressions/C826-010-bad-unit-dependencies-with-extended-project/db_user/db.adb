with Text_Io; use Text_Io;
with Fileio;
package body Db is
  procedure Open is
  begin
    Put_Line ("open db (user)");
    Fileio.Open;
  end Open;
end Db;

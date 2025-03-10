package body FM.System_Dependant is

   function Thread_Setup return System.Task_Info.Task_Info_Type is
   begin
      return System.Task_Info.Unspecified_Task_Info;
   end;
end  FM.System_Dependant;

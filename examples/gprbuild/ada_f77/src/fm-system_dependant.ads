with System.Task_Info;
package FM.System_Dependant is

   function Thread_Setup return System.Task_Info.Task_Info_Type;
   --  This function is called at elaboration of the Worker's tasks
   --  and can be used to set some system specific options

end  FM.System_Dependant;


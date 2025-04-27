with Ada.Text_IO;
with Log_Level; use Log_Level;

package body Log is
    procedure Debug (Message : String) is
    begin
        if Local_Log_Level <= Log_Level.Debug then
            Write_Log ("[Debug]: " & Message);
        end if;
    end Debug;

    procedure Info (Message : String) is
    begin
        if Local_Log_Level <= Log_Level.Info then
            Write_Log ("[Info]: " & Message);
        end if;
    end Info;

    procedure Error (Message : String) is
    begin
        --  if Local_Log_Level <= Log_Level.Error then
        --      Write_Log (Message);
        --  end if;
        Write_Log ("[Error]: " & Message);
    end Error;

    procedure Write_Log (Message : String) is
    begin
        Ada.Text_IO.Put_Line (Message);
    end Write_Log;
end Log;


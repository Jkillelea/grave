with Log_Level;

--  A logging implementation that lets the log level for different packages be adjusted independently.
generic
    Local_Log_Level : Log_Level.Log_Level := Log_Level.Error;
package Log is

    procedure Debug (Message : String);
    procedure Info  (Message : String);
    procedure Error (Message : String);

private
    -- This should later be extended to be able to write to logfiles, but now it just prints to the console.
    procedure Write_Log (Message : String);
end Log;

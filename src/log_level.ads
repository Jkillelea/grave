--  This package only exists to pre-declare the Log_Level type
package Log_Level is
    pragma Pure (Log_Level);

    type Log_Level is (Debug, Info, Error);
    for Log_Level use (
        Debug => 0,
        Info  => 1,
        Error => 2
        );
end Log_Level;

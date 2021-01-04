--
-- Module containing the user interface functions/procedures
--

package console is
    NBOPTION: Integer := 16;
    --------------------------------------------------------
    -- procedure show_options
    -- sémantique: show all the available user options
    -- parameters: none
    -- return type: none
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    procedure show_options_console;

    --------------------------------------------------------
    -- procedure choose_option_console (recursive)
    -- sémantique: grab the user response 
    -- parameters: none
    -- return type: The id of the action to execute
    -- pre-condition: none
    -- post-condition: none
    -- exception: BAD_CHOICE
    function choose_option_console return Integer;
end console;
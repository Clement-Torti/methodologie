--
-- Module containing the user interface functions/procedures
--

package console is
    NBOPTION: Integer := 16;
    BAD_CHOICE: Exception;
    
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
    -- exception: BAD_CHOICE DATA_ERROR
    function choose_option_console return Integer;

    --------------------------------------------------------
    -- procedure choose_id_console
    -- sémantique: provide a uniform way of selected someone based on his id
    -- parameters: none
    -- return type: Integer: The id
    -- pre-condition: none
    -- post-condition: none
    -- exception: DATA_ERROR
    function choose_id_console return Integer;

    --------------------------------------------------------
    -- procedure choose_gen_console
    -- sémantique: provide a uniform way of choosing a generation
    -- parameters: none
    -- return type: Integer: The generation
    -- pre-condition: none
    -- post-condition: none
    -- exception: BAD_CHOICE DATA_ERROR
    function choose_gen_console return Integer;


end console;
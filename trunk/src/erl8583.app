{application, erl8583,
    [
        {description, "Erl8583 library for ISO 8583"},
        {vsn, "1.0.0"},
        {modules,   [
                    erl8583_convert,
                    erl8583_fields,
                    erl8583_fields_1993,
                    erl8583_fields_2003,
                    erl8583_marshaller,
                    erl8583_marshaller_ascii,
                    erl8583_marshaller_binary,
                    erl8583_marshaller_ebcdic,
                    erl8583_marshaller_xml,
                    erl8583_message,
                    erl8583_message_helpers
                    ]},
        {registered, []},
        {env, []},
        {applications, [kernel, stdlib]}
    ]
}.

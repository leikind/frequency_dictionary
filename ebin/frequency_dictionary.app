{application, frequency_dictionary,
 [{description, "A frequency dictionary for multiple languages"},
  {vsn, "0.1.0"},
  {modules, [
              frequency_dictionary_app,
              frequency_dictionary_sup,
              frequency_dictionary_server,
              frequency_dictionary_pub
            ]},
  {registered, [frequency_dictionary_sup]}, % ???
  {applications, [kernel, stdlib]},
  {mod, {frequency_dictionary_app, [[en, nl, fr]]}}
 ]}.
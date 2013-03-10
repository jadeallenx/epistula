%% MIME message structure

-record(mime_msg, {
    type=mixed, 
    headers=[], 
    boundary, 
    parts=[]}
).

-record(mime_part, { 
    encoding={"7bit", "text/plain", "US-ASCII"}, 
    disposition=inline, 
    filename, 
    data}
).

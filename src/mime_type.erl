% @doc
% Given a file extension, return a MIME type.
%
% Primary source for these comes from
% [http://svn.apache.org/repos/asf/httpd/httpd/branches/1.3.x/conf/mime.types]
% @end
-module(mime_type).
-export([extension/1]).

-spec extension( Extension :: string() ) -> string().
% @doc Get a MIME type given a file extension.
%
% The default MIME type is `application/octet-stream'
% @end
%
% certificates/crypto
extension("crt") -> "application/x-x509-ca-cert";
extension("cer") -> "application/pkix-cert";

% postscript 
extension("pdf") -> "application/pdf";
extension("ps")  -> "application/postscript";

% audio
extension("ogg") -> "audio/ogg";
extension("mp3") -> "audio/mpeg";
extension("wav") -> "audio/vnd.wave";
extension("aac") -> "audio/x-aac";
extension("au") -> "audio/basic";
extension("snd") -> "audio/basic";

% video
extension("mov") -> "video/quicktime";
extension("mp4") -> "video/mp4";
extension("wmv") -> "video/x-ms-wmv";
extension("avi") -> "video/x-msvideo";

% image
extension("jpg") -> "image/jpeg";
extension("jpeg") -> "image/jpeg";
extension("png") -> "image/png";
extension("gif") -> "image/gif";
extension("tiff") -> "image/tiff";

% OpenDocument
extension("odt") -> "application/vnd.oasis.opendocument.text";
extension("ods") -> "application/vnd.oasis.opendocument.spreadsheet";
extension("odp") -> "application/vnd.oasis.opendocument.presentation";

% MS
extension("doc") -> "application/vnd.ms-word";
extension("xls") -> "application/vnd.ms-excel";
extension("ppt") -> "application/vnd.ms-powerpoint";
extension("docx") -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
extension("xlsx") -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
extension("pptx") -> "application/vnd.openxmlformats-officedocument.presentationml.presentation";

%% archive / compression
extension("zip") -> "application/zip";
extension("bz2") -> "application/x-bzip2";
extension("gz") -> "application/gzip";
extension("tar") -> "application/x-tar";
extension("jar") -> "application/java-archive";

% html
extension("html") -> "text/html";
extension("htm") -> "text/html";
extension("css") -> "text/css";

%% text
extension("txt") -> "text/plain";
extension("csv") -> "text/csv";
extension("ics") -> "text/calendar";

extension(_) -> "application/octet-stream".

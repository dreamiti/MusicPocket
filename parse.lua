--SSYTVP27

if DEBUG == nil then
	DEBUG = 0
end
if APP_VERSION == nil then
	APP_VERSION = 0
end
if IOS_VERSION == nil then
	IOS_VERSION = "unknown"
end

-- for json.decode
local type = type;
local t_insert, t_concat, t_remove, t_sort = table.insert, table.concat, table.remove, table.sort;
local s_char = string.char;
local tostring, tonumber = tostring, tonumber;
local pairs, ipairs = pairs, ipairs;
local next = next;
local error = error;
local newproxy, getmetatable = newproxy, getmetatable;
local print = print;

--module("json")
local json = {};

local null = newproxy and newproxy(true) or {};
if getmetatable and getmetatable(null) then
	getmetatable(null).__tostring = function() return "null"; end;
end
json.null = null;

local escapes = {
	["\""] = "\\\"", ["\\"] = "\\\\", ["\b"] = "\\b",
	["\f"] = "\\f", ["\n"] = "\\n", ["\r"] = "\\r", ["\t"] = "\\t"};
local unescapes = {
	["\""] = "\"", ["\\"] = "\\", ["/"] = "/",
	b = "\b", f = "\f", n = "\n", r = "\r", t = "\t"};
for i=0,31 do
	local ch = s_char(i);
	if not escapes[ch] then escapes[ch] = ("\\u%.4X"):format(i); end
end

function dlog(s)
	if DEBUG == 1 then
		print(s)
	end
end

function json.decode(json)
	json = json.." "; -- appending a space ensures valid json wouldn't touch EOF
	local pos = 1;
	local current = {};
	local stack = {};
	local ch, peek;
	local function next()
		ch = json:sub(pos, pos);
		if ch == "" then error("Unexpected EOF"); end
		pos = pos+1;
		peek = json:sub(pos, pos);
		return ch;
	end
	
	local function skipwhitespace()
		while ch and (ch == "\r" or ch == "\n" or ch == "\t" or ch == " ") do
			next();
		end
	end
	local function skiplinecomment()
		repeat next(); until not(ch) or ch == "\r" or ch == "\n";
		skipwhitespace();
	end
	local function skipstarcomment()
		next(); next(); -- skip '/', '*'
		while peek and ch ~= "*" and peek ~= "/" do next(); end
		if not peek then error("eof in star comment") end
		next(); next(); -- skip '*', '/'
		skipwhitespace();
	end
	local function skipstuff()
		while true do
			skipwhitespace();
			if ch == "/" and peek == "*" then
				skipstarcomment();
			elseif ch == "/" and peek == "/" then
				skiplinecomment();
			else
				return;
			end
		end
	end
	
	local readvalue;
	local function readarray()
		local t = {};
		next(); -- skip '['
		skipstuff();
		if ch == "]" then next(); return t; end
		t_insert(t, readvalue());
		while true do
			skipstuff();
			if ch == "]" then next(); return t; end
			if not ch then error("eof while reading array");
			elseif ch == "," then next();
			elseif ch then error("unexpected character in array, comma expected"); end
			if not ch then error("eof while reading array"); end
			t_insert(t, readvalue());
		end
	end
	
	local function checkandskip(c)
		local x = ch or "eof";
		if x ~= c then error("unexpected "..x..", '"..c.."' expected"); end
		next();
	end
	local function readliteral(lit, val)
		for c in lit:gmatch(".") do
			checkandskip(c);
		end
		return val;
	end
	local function readstring()
		local s = "";
		checkandskip("\"");
		while ch do
			while ch and ch ~= "\\" and ch ~= "\"" do
				s = s..ch; next();
			end
			if ch == "\\" then
				next();
				if unescapes[ch] then
					s = s..unescapes[ch];
					next();
				elseif ch == "u" then
					local seq = "";
					for i=1,4 do
						next();
						if not ch then error("unexpected eof in string"); end
						if not ch:match("[0-9a-fA-F]") then error("invalid unicode escape sequence in string"); end
						seq = seq..ch;
					end
					s = s..s.char(tonumber(seq, 16)); -- FIXME do proper utf-8
					next();
				else error("invalid escape sequence in string"); end
			end
			if ch == "\"" then
				next();
				return s;
			end
		end
		error("eof while reading string");
	end
	local function readnumber()
		local s = "";
		if ch == "-" then
			s = s..ch; next();
			if not ch:match("[0-9]") then error("number format error"); end
		end
		if ch == "0" then
			s = s..ch; next();
			if ch:match("[0-9]") then error("number format error"); end
		else
			while ch and ch:match("[0-9]") do
				s = s..ch; next();
			end
		end
		if ch == "." then
			s = s..ch; next();
			if not ch:match("[0-9]") then error("number format error"); end
			while ch and ch:match("[0-9]") do
				s = s..ch; next();
			end
			if ch == "e" or ch == "E" then
				s = s..ch; next();
				if ch == "+" or ch == "-" then
					s = s..ch; next();
					if not ch:match("[0-9]") then error("number format error"); end
					while ch and ch:match("[0-9]") do
						s = s..ch; next();
					end
				end
			end
		end
		return tonumber(s);
	end
	local function readmember(t)
		skipstuff();
		local k = readstring();
		skipstuff();
		checkandskip(":");
		t[k] = readvalue();
	end
	local function fixobject(obj)
		local __array = obj.__array;
		if __array then
			obj.__array = nil;
			for i,v in ipairs(__array) do
				t_insert(obj, v);
			end
		end
		local __hash = obj.__hash;
		if __hash then
			obj.__hash = nil;
			local k;
			for i,v in ipairs(__hash) do
				if k ~= nil then
					obj[k] = v; k = nil;
				else
					k = v;
				end
			end
		end
		return obj;
	end
	local function readobject()
		local t = {};
		next(); -- skip '{'
		skipstuff();
		if ch == "}" then next(); return t; end
		if not ch then error("eof while reading object"); end
		readmember(t);
		while true do
			skipstuff();
			if ch == "}" then next(); return fixobject(t); end
			if not ch then error("eof while reading object");
			elseif ch == "," then next();
			elseif ch then error("unexpected character in object, comma expected"); end
			if not ch then error("eof while reading object"); end
			readmember(t);
		end
	end
	
	function readvalue()
		skipstuff();
		while ch do
			if ch == "{" then
				return readobject();
			elseif ch == "[" then
				return readarray();
			elseif ch == "\"" then
				return readstring();
			elseif ch:match("[%-0-9%.]") then
				return readnumber();
			elseif ch == "n" then
				return readliteral("null", null);
			elseif ch == "t" then
				return readliteral("true", true);
			elseif ch == "f" then
				return readliteral("false", false);
			else
				error("invalid character at value start: "..ch);
			end
		end
		error("eof while reading value");
	end
	next();
	return readvalue();
end

function split(str, pat)
	if str == nil or pat == nil then
		return nil
	end

	local t = {}  -- NOTE: use {n = 0} in Lua-5.0
    local fpat = "(.-)" .. pat
    local last_end = 1
    local s, e, cap = str:find(fpat, 1)
    while s do
        if s ~= 1 or cap ~= "" then
	        table.insert(t,cap)
        end
        last_end = e+1
        s, e, cap = str:find(fpat, last_end)
    end
    if last_end <= #str then
        cap = str:sub(last_end)
        table.insert(t, cap)
    end
    return t
end

function split2(str, char)
	if str == nil or char == nil then
		return nil
	end

	local s, e = string.find(str, char)
	if s == nil then
		return nil
	end

	local first = string.sub(str, 1, s-1)
	local second = string.sub(str, e+1)
	if first ~= nil and second ~= nil then
		local t ={}
		table.insert(t, first)
		table.insert(t, second)
		return t
	else
		return nil
	end
end

function url_decode(url)
	if url == nil then
		return nil
	end
	local s = string.gsub(url, "%%3B", ";")
	s = string.gsub(s, "%%3F", "?")
	s = string.gsub(s, "%%2F", "/")
	s = string.gsub(s, "%%3A", ":")
	s = string.gsub(s, "%%23", "#")
	s = string.gsub(s, "%%26", "&")
	s = string.gsub(s, "%%3D", "=")
	s = string.gsub(s, "%%2B", "+")
	s = string.gsub(s, "%%24", "$")
	s = string.gsub(s, "%%2C", ",")
	s = string.gsub(s, "%%20", "+")
	s = string.gsub(s, "%%25", "%%")
	s = string.gsub(s, "%%3C", "<")
	s = string.gsub(s, "%%3E", ">")
	s = string.gsub(s, "%%7E", "~")
	--s = string.gsub(s, "\\u0026", "&")
	return s
end

function decode_escape_sequences(text)
	if text == nil then
		return nil
	end
	local s = string.gsub(text, "\\\\", "\\")
	s = string.gsub(s, "\\\\%/", "/")
	s = string.gsub(s, "\\\"", "\"")
	s = string.gsub(s, "\\u0026", "&")
	return s
end

function remove_double_quotation(text)
	if text == nil then
		return nil
	end
	local startIndex, endIndex, token = string.find(text, "[^\"]*\"(.-)\"")
	return token
end

function is_3d_video(item)
	if item["stereo3d"] then
		return 1
	end
	if item["itag"] == "82" then
		return 1
	end
	if item["itag"] == "83" then
		return 1
	end
	if item["itag"] == "84" then
		return 1
	end
	if item["itag"] == "85" then
		return 1
	end

	return 0
end

function is_available_video(item)
	if is_3d_video(item) == 1 then
		return 0
	end

	return 1
end

function get_key_value_pairs(text)
	if text == nil then
		return nil
	end

	local keyValuePairs = split(text, "&")
	local dstItems = {}
	for j = 1, #keyValuePairs, 1 do
		local keyValuePair = keyValuePairs[j]
		local keyAndValue = split2(keyValuePair, "=")
		if keyAndValue ~= nil and #keyAndValue >= 2 then
			local key = keyAndValue[1]
			local encodedValue = keyAndValue[2]
			--if decodeKeys[key] == 1 then
			local value = url_decode(encodedValue)
			--end

			dstItems[key] = value
		end
	end

	-- for key, value in pairs(dstItems) do
	-- 	dlog(key .. "=" .. value)
	-- end

	return dstItems
end

function parse_get_video_info(text)
	if text == nil then
		return nil
	end

	local token = string.find(text, "url_encoded_fmt_stream_map")
	if token == nil then
		dlog("Not found url_encoded_fmt_stream_map in get_video_info")
		return nil
	end

	local items = get_key_value_pairs(text)
	if items == nil or items["url_encoded_fmt_stream_map"] == nil then
		dlog("Not found url_encoded_fmt_stream_map in key_value_pairs of get_video_info")
		return nil
	end

	local stream_map = items["url_encoded_fmt_stream_map"]
	local stream_items = split(stream_map, ",")
	if stream_items == nil then
		dlog("Could not parse url_encoded_fmt_stream_map of get_video_info")
		return nil
	end

	local dstItems = {}
	for i = 1, #stream_items, 1 do
		local kv = get_key_value_pairs(stream_items[i])
		table.insert(dstItems, kv)
	end

	if #dstItems == 0 then
		return 0
	end

	return dstItems
end

function parse_html(text)
	if text == nil then
		return nil
	end

	local startIndex, endIndex

    -- look for json data like '"url_encoded_fmt_stream_map": "...http=//....,...http=//......",'
	local token = string.find(text, "\"url_encoded_fmt_stream_map\".-:.-\"([^\"]*)")
	if token == nil then
		dlog("Not found url_encoded_fmt_stream_map in html")
		return nil
	end

	local srcItems = split(token, ",")
	if srcItems == nil then
		dlog("Not found srcItems")
		return nil
	end

	local decodeKeys = {}
	decodeKeys["url"] = 1
	decodeKeys["type"] = 1

	local dstItems = {}

	for i = 1, #srcItems, 1 do
		local keyValuePairs = split(srcItems[i], "\\u0026")
		local dstItem = {}
		for j = 1, #keyValuePairs, 1 do
			local keyValuePair = keyValuePairs[j]
			local keyAndValue = split(keyValuePair, "=")
			local key = keyAndValue[1]
			local value = keyAndValue[2]
			if decodeKeys[key] == 1 then
				value = url_decode(value)
			end

			dstItem[key] = value
		end
		dstItems[i] = dstItem
	end

	if #dstItems == 0 then
		return nil
	end
	return dstItems
end

function parse_mobile_html(text)
	if text == nil then
		return nil
	end
    -- look for json data like '"fmt_stream_map": [{"url"="http=//....",...},{...}],'
	local startIndex, endIndex, token = string.find(text, '%\"fmt_stream_map[^%[]*%[([^%]]*)')
	if token == nil then
		return nil
	end

	token = "{\"items\":[" .. token .. "]}"
	token = decode_escape_sequences(token)

	obj = json.decode(token)
	if obj == nil then
		return nil
	end

	local dstItems = {}

	for key, value in pairs(obj) do
		items = value
		for i = 1, #items do
			item = items[i]
			local dstItem = {}
			for k, v in pairs(item) do
				dstItem[k] = v
			end
			dstItems[i] = dstItem
		end
	end

	if #dstItems == 0 then
		return nil
	end
	return dstItems
end

function get_signature_code(text)
	local token = string.match(text, '%.signature=([^(]+)%(')
	if token == nil then
		return nil
	end
	return token
end

function convertItems(items, signatureCode)
	if items == nil then
		return nil
	end

	local dstItems = {}
	local dstItemIndex = 1
	for i = 1, #items, 1 do
		local item = items[i]
		local dstItem = {}

		if is_available_video(item) == 1 then

			if item["sig"] then
				dstItem["url"] = item["url"] .. "&signature=" .. sig(item["sig"])

				if signatureCode then
					dstItem["signatureCode"] = signatureCode .. "('" .. item["sig"] .. "');"
					dstItem["signatureArg"] = "signature"
				end
			else
				dstItem["url"] = item["url"]
			end

			if item["type"] then
				local startIndex, endIndex, videoType = string.find(item["type"], "([^;]*)")
				dstItem["type"] = videoType
			else
				dstItem["type"] = "unknown"
			end

			dstItem["quality"] = item["quality"]
			dstItems[dstItemIndex] = dstItem
			dstItemIndex = dstItemIndex + 1
		end
	end
	return dstItems
end

function sig(s)
	dlog("signature=".. s .. " len="..string.len(s))
	if string.len(s) == 81 then
		return sig81(s)
	elseif string.len(s) == 82 then
		return sig82(s)
	elseif string.len(s) == 83 then
		return sig83(s)
	elseif string.len(s) == 85 then
		return sig85(s)
	elseif string.len(s) == 86 then
		return sig86(s)
	elseif string.len(s) == 88 then
		return sig88(s)
	elseif string.len(s) == 91 then
		return sig91(s)
	elseif string.len(s) == 93 then
		return sig93(s)
	else
		return s
	end
end

function slice(s, i)
	return string.sub(s, i + 1)
end

function Zo(a,b)
	len = string.len(a)
	b = b % len
	b = b + 1
	local c = string.sub(a,1,1)
	local s0 = string.sub(a,b,b)
	local s1 = string.sub(a,2,b-1)
	local s2 = string.sub(a,b+1)
	s = s0 .. s1 .. c .. s2
	return s
end

function sig81(s)
	local a = s;
	a = Zo(a, 50);
	a = Zo(a, 17);
	a = string.reverse(a);
	a = Zo(a, 7);
	a = Zo(a, 65);
	return a
end

function sig82(s)
	local a = Zo(s, 7);
	a = Zo(a, 37)
	a = string.reverse(a);
	a = slice(a, 1);
	return a
end

function sig83(s)
	local a = s;
	a = slice(a, 1);
	a = string.reverse(a);
	a = Zo(a, 41);
	a = string.reverse(a);
	a = Zo(a, 41);
	a = slice(a, 1);
	a = Zo(a, 15);
	return a
end

function sig85(s)
	local a = s;
	a = Zo(a, 3);
	a = Zo(a, 7);
	a = string.reverse(a);
	a = slice(a, 2);
	a = Zo(a, 27);
	a = slice(a, 2);
	a = Zo(a, 42);
	a = string.reverse(a);
	return a
end

function sig86(s)
	local a = s;
	a = slice(a, 2);
	a = Zo(a, 55);
	a = Zo(a, 10);
	a = slice(a, 3);
	a = Zo(a, 57);
	a = string.reverse(a);
	a = Zo(a, 25);
	a = Zo(a, 41);
	return a
end

function sig88(s)
	local a = s;
	a = Zo(a, 2);
	a = string.reverse(a);
	a = slice(a, 3);
	a = Zo(a, 52);
	a = slice(a, 2);
	a = Zo(a, 63);
	a = slice(a, 2);
	return a
end

function sig91(s)
	local a = s;
	a = string.reverse(a);
	a = slice(a, 3);
	a = Zo(a, 49);
	a = slice(a, 3);
	a = string.reverse(a);
	a = Zo(a, 58);
	a = slice(a, 2);
	a = string.reverse(a);
	a = slice(a, 2);
	return a
end

function sig93(s)
	local d = string.sub(s,7,-7)
	local c = string.sub(s,-5, -5)
	d = string.reverse(d)
	d = string.sub(d,1,57) .. c .. string.sub(d,59)
	return d
end

function jsonFromArray(array, name)
	if array == nil or name == nil then
		return ""
	end

	local json = "\"" .. name .. "\" : [\n"
	for i = 1, #array, 1 do
		if i > 1 then
			json = json .. ",\n"
		end
		json = json .. jsonFromDictionary(array[i])
	end
	json = json .. "\n]"
	return json
end

function jsonFromDictionary(d)
	if d == nil then
		return ""
	end

	local json = "{\n"
	local i = 0
	for key, value in pairs(d) do
		if i > 0 then
			json = json .. ",\n"
		end
	    json = json .. "\"" .. key .. "\" : \"" .. value .. "\""
		i = i + 1
	end

	json = json .. "\n}"
	return json
end

function getItemsJSON(html)
	if html == nil then
		return nil
	end

	local items = nil
	local signatureCode = nil

	--if APP_VERSION >= 1 then
	--	items = parse_get_video_info(html)
	--end

	-- if items == nil then
	-- 	items = parse_html(html)
	-- end
	if items == nil then
		items = parse_mobile_html(html)
		signatureCode = get_signature_code(html)
	end

	if items == nil then
		return nil
	end

	if signatureCode ~= nil then
		dlog("signatureCode="..signatureCode)
	else
		dlog("signatureCode=none")
	end

	-- print (jsonFromArray(items, "items"))

	local convertedItems = convertItems(items, signatureCode)
	if convertedItems == nil then
		return nil
	end

	-- print (jsonFromArray(convertedItems, "items"))

	local json = "{\n"
	json = json .. jsonFromArray(convertedItems, "items")
	json = json .. "\n}"
	return json
end

dlog("AppVersion=" .. APP_VERSION .. " iOS=" .. IOS_VERSION)

return getItemsJSON(g_html)

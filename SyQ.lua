--   SyQ.lua ???
--   by E. Rosa
--[[
  Copyright (c) 2013 Hector (Eitan) Rosa

	Permission is hereby granted, free of charge, to any person obtaining a
	copy of this software and associated documentation files (the
	"Software"), to deal in the Software without restriction, including
	without limitation the rights to use, copy, modify, merge, publish,
	distribute, sublicense, and/or sell copies of the Software, and to
	permit persons to whom the Software is furnished to do so, subject to
	the following conditions:

	The above copyright notice and this permission notice shall be included
	in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
	OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
	CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
	TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
	SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--]]

class = require '30log'
local _ = require 'moses'
Fn = _ -- aliasing???
-- Fn.import()
-- print(Fn.identity(1)) Just checking!!!
-- lambda = require("lambda")
PEG = require("PureLPeg")
--LPeg = require("lpeg")
--LFS = require("luafilesystem") 
tincan = require 'tincan'
LFlag = 0
--CurlyCounter = 0; ParensCounter = 0; 			-- Likely to be eliminated, not needed???

T = {}; t = {}; R = {}; C = {}; O = {}; H = {}; L = {};
L = lambda
--L.map = lambda.map
--L.filter= lambda.filter
--L.reduce = lambda.reduce
A = {}; A[0] = "Array module, for matrix functionally";
V = {};	V[0] = "Vector module, for vector functionality";
F = {}; 
W = {}; W[0] = "main table for mutable, runtime variables and tables, workspace";
Z = {};	-- primitives here!
SQL = {}; S = {};  -- coming soon????

Class = class()

Object = Class:extends { doc = "I am the root Object" }

function Object:__init(doc)
	self.super.__init(self, doc)
	self.doc = doc
	self.dress = nil
end
-- Handy/efficient alternative implementation for Queues, Deques, Stacks, Lists
-- single inheritance version using 30log for class/object system implementation
-- derived from PiL (Programming in Lua, R. Yerishalmischy) 
-- TODO use for X and both X, Y to compare performance!
List = Object:extends { first = 0, last = -1, data = {}, doc = 'I can implement lists, queues and deques', }

function List:__init(first,last, data,doc)
--	self.super.__init(self,doc)
	self.fist = first
	self.last = last
	self.data = data
	self.doc = doc
	self.dress = "list"
end

function List:pushFirst(value)
	local first = self.first - 1
	self.first = first
	self.data[first] = value
end

function List:pushLast(value)
	local last = self.last + 1
	self.last = last
	self.data[last] = value
end

function List:popFirst()
	local first = self.first
	if first > self.last then error("list is empty") end
	local value = self.data[first]
	self.data[first] = nil  	-- set for garbage collection
	self.first = first + 1
	return value
end
	
function List:popLast()
	local last = self.last
	if self.first > last then error("list is empty") end
	local value = self.data[last]
	self.data[last] = nil	-- set for garbage collection
	self.last = last - 1
	return value
end
	
Stack = Object:extends { data = {} }

function Stack:__init(data)
	self.data = data
	self.dress = "stack"
end

function Stack:push(v)     --to operate on any s passed as first arg
	table.insert(self.data,v)
end;

function Stack:push2(...)     --to operate on any s passed as first arg
	local v1, v2 = ...
	table.insert(self.data,v1)
	table.insert(self.data,v2)
end;

function Stack:pushLast(v)  --pushBottom --to operate on any self.data paself.datased as first arg
	if v then
		table.insert(self.data,1,v) ; -- io.write("pushed in queue ", v); -- print('\n')
	end
end;

function Stack:popBottom()    --to operate on passed as first arg
	return table.remove(self.data,1)
end;

function Stack:pop() --to operate on any self.data passed as first arg
	return table.remove(self.data)
end;

function Stack:FLUSH()
	self.data = {}
end;

function Stack:peek()
	local tos = self:pop()
	self:push(tos)
	return tos
end;

function Stack:peekNOS()
	local nos = self.data[#self.data - 1]
	return nos
end;

function Stack:peekZOS()
	local zos = self.data[#self.data - 2]
	return zos
end;

function Stack:dup()
	self:push(self:peek())
end;

function Stack:drop()
	local dummy = table.remove(self.data)
end;

function Stack:swap()
	local tos = self:pop()
	local nos = self:pop()
	self:push(tos)
	self:push(nos)
end;

function Stack:over()
	local tos = self:pop()
	local nos = self:pop()
	self:push(nos)
	self:push(tos)
	self:push(nos)
end;

function Stack:rot()
	local tos = self:pop()
	local nos = self:pop()
	local zos = self:pop()
	self:push(nos)
	self:push(tos)
	self:push(zos)
end;

function Stack:dip(depth) 
	local x = self.data[#self.data - depth]
	return x
end;

function Stack:nip(depth) 
	local val = self.data[#self.data - depth]
	for ix = depth, 1 do
		self.data[#self.data - ix] = self.data[#self.data - ix + 1]
	end
	self.data[#self.data] = nil
    return val
end;

function Stack:tuck(depth,val) 
	for ix = depth, 0 do
		self.data[#self.data - ix + 1] = self.data[#self.data - ix]
	end
	self.data[#self.data - depth] = val
end;

function Stack:swaps() -- use reverse???
	local temps = {}
	temps = Fn.reverse(self.data, iter)
	self.data = temps
	return 
end;

function Stack:dups()
	local sz = #self.data
	for ix = 1, sz do
		self:push(self.data[ix])
	end
end

function Stack:printSelf()  -- needs testing
	---local stk = ...
	local stk = self:swaps() -- reverse temp s to enable easy top to bottom listing of s contents
	if #stk.data < 1 then
		io.write('Empty Stack!\n')
	else
		for ix = 1, #stk.data do
			if type(stk.data[ix]) == "table" then
        io.write(' ')
				V.print(stk.data[ix])
				io.write('\n')
			else
				io.write(stk.data[ix])
				io.write('\n')
			end
		end
	end
end;

function Stack:printTOS()
	local size = #self.data
	local tos
	if size < 1 then
		io.write("Stack Underflow!\n")
	else
		tos = self.data[#self.data]
		self.data[#self.data] = nil
		if type(tos) == "table" then
      V.print(tos)
		else
			io.write(tostring(tos),'\n')
		end
	end
end;

function Stack:size()
	local sz = #self.data
	return sz
end

function Stack:sizeToTOS()
    local size = #self.data
	return self:push(size)
end;

function Stack:infoTOS()
	local obj = self:peek()
	local result = {}
	result[1] = type(obj)
	if obj.dress then 
		result[2] = obj.dress
	end
    if obj.dim then 
		result[3] = obj.dim
	end
	return result
end

X = Stack:new({})  	-- Data stack
Y = Stack:new({})	-- Deque/Queue  try Y = List:new() ???

SwitchTable = Object:extends { switch = {case = nil, code = nil}, doc = 'I can implement lists, queues and deques', }

function SwitchTable:__init(switch, doc)
  self.switch = switch
	self.doc = doc
	self.dress = "switch"
end

function SwitchTable:addCase(c,l)
	local t = {}
	t = {case = c, code = code}
	table.insert(self.switch, t)
end 

function SwitchTable:eval(cond)
	local result
	for k,v in self.switch do
		if self.switch[k].case == cond then
			result = self[k].code
			break
		end
	end
	while(result) do
		Y:pushTop(table.remove(result))
	end
end
		
-- Thread/coroutine "center" here
-- tree "home" for coroutines
r = {}; r[0] = "Table for 'parking' coroutines";
R = coroutine; 

SetNewMacro = function(...)
	local isKind, lambda, name = ...
	if isKind == "Lua" then
		Lua[name] = lambda
	else
		R[name] = lambda
	end  -- if
end;

C.doIf = function(...)
     local lfalse, ltrue, cond, flag = ...  -- flag SHOULD be nil. just declared, at this point !! just a LOC saver!
     X:push(cond:exec())
     flag = X:pop()
     if flag then
          ltrue:exec()
     else
          lfalse:exec()
     end -- if
end;

-- need to do! make a 'cases table' key -> case, value -> lambda::function
-- kvTable -> [[ c1 {lambda1}][ c2 {lambda2}][ c3 {lambda3}]...[ cN {lambdaN}]]
C.switch = function(...) 	
	local kvTable, condn = ...  -- figure out use of default!
	X:push(condn)  -- fix from here?
	if kvTable[condn] then
		P(kvTable[condn])
--else
-- 	P(lfalse)
	end -- if
end;

C.loop = function(...)
	local lambda, cond, flag = ...
	if not flag then
		repeat
			lambda:exec()
		until ( cond:exec() ~= true);
	else
		while (cond:exec()) do
			lambda:exec()
		end -- while
	end -- if
end ;

O.applyArithOps = function (...)
	local op, a, b = ...
	local result
	if I.nums(a,b) then
		if     op == "+" then
			result = O.add(tonumber(a), tonumber(b))
		elseif op == "-" then
			result = O.sub(tonumber(a), tonumber(b))
		elseif op == "*" then
			result = O.mul(tonumber(a), tonumber(b))
		elseif op == "%" then
			result = O.div(tonumber(a), tonumber(b))
		elseif op == "^" then
			result = O.pow(tonumber(a), tonumber(b))
		end
		return result
	elseif (not(I.atom(a)) or not(I.atom(b))) then
		if op == "*" then      
			if  I.atom(a) then -- check for multiDim recursive?
				result = {}
				local ia = V.newSameValue(a,#b)
				for ii = 1, #b do result[ii] = ia[ii]*b[ii] end
			elseif I.atom(b) then
				result = {}
				local ib = V.newSameValue(b,#a)
				for ii = 1, #a do result[ii] = ib[ii]*a[ii] end
			elseif A.matchMult(b,a) then
				result = {}
				result = A.multM(b,a)
			elseif V.sameSize(b,a) then
				result = V.dot(b,a)			
			else
				io.write("invalid operation, incompatible dimensions on arrays!\n")
			end
		elseif op == "+" then
			if I.atom(a) then
				result = {}
				local ia = V.newSameValue(a,#b)
				for ii = 1, #b do 
					if type(b[ii]) == 'table' then 
						result[ii] = O.applyArithOps('+',ia[ii],b[ii]) 
					else result[ii] = ia[ii] + b[ii] end
				end
			elseif I.atom(b) then
				result = {}
				local ib = V.newSameValue(b,#a)
				for ii = 1, #a do 
					if type(a[ii]) == 'table' then 
						result[ii] = O.applyArithOps('+',ib[ii],a[ii])
					else result[ii] = ib[ii]+a[ii] end
				end
			elseif A.sameSize(b,a) then
				result = {} 
				result = A.add(b,a)
			else 
				io.write("invalid operation, incompatible dimensions on arrays!\n")
			end
		end
		return result
	end
end

F.mean = function(...)
	local vector = ...
	local result
	if type(vector) == "table" then
		result = V.reduce("+", vector, 0) / #vector
		-- result = Fn.
	elseif tonumber(vector) then
		result = vector
	else
		io.write("Illegal Parameters")
		result = "Error"
	end
	return result
end

F.median = function(...)
	local vector = ...
	local ix = #vector / 2
	local tvector = vector
	local result
	table.sort(tvector)
	if ix == math.floor(ix) then
		result = (tvector[ix] + tvector[ix + 1]) / 2
	else
		ix = math.floor(ix + 1)
		result = tvector[ix]
	end
	return result
end

F.fac = function(...)
	local num = ...
	local iotaFromNum = {}
	local result                 --	pushTop(X, num)
	iotaFromNum = V.iota(num)
	result = V.reduce("*", iotaFromNum, 1)
	return result
end

F.facOrExec = function(...)
	local obj = ...
	if tonumber(obj) then
		S.push(F.fac(obj))
	elseif C[obj] then
		P(C[obj])
	elseif L[obj] then
		assert(loadstring(L[obj]))()
	elseif Q[obj] then
		io.write("DB bindings not implemented yet")
	elseif obj.dress == 'l' then
		--obj:buildAsString()
		obj:exec()
	else
		io.write("Incompatible Objects found for operator!")
	end
end

V.iota = function(...)
	local isize = tonumber(...)
	local it = {}
	if isize < 1 then
		io.write("invalid value for function!\n")
	elseif isize == 1 then
		it = {1}
	else
		for ix = 1, isize do
		   table.insert(it, ix)
		end
	end
	return it
end

V.iota0 = function(...)
	local isize = tonumber(...)
	local it = {}
	if isize < 1 then
		io.write("invalid value for function!\n")
	elseif isize == 1 then
		it = {0}
	else
		for ix = 0, isize - 1 do
		   table.insert(it, ix)
		end
	end
	return it
end

V.sort = function(...)
	local direction, vector = ... -- values for direction {ascending, descending} az za
	if direction == "az" then		-- get a quicksort??? not!!! thanks Lua!
		table.sort(vector)
	elseif direction == "za" then
		table.sort(vector, function(a,b) return a>b end)
	end
	return X:push(vector)
end

O.gt = function(b, a) return a > b end
O.lt = function(b, a) return a < b end
O.eq = function(b, a) return a == b end
O.ge = function(b, a) return a >= b end
O.le = function(b, a) return a <= b end
O.ne = function(b, a) return a ~= b end
O.AND = function(b, a) return (a and b) end
O.OR = function(b, a) return (a or b) end
O.NOT = function(a) return (not a) end
O.XOR = function(b, a) return((a or b) and ( not(a and b)))end  
O.mul = function(a, b) return tonumber(a) * tonumber(b) end
O.add = function(a, b) return tonumber(a) + tonumber(b) end
O.sub = function(a, b) return tonumber(a) - tonumber(b) end
O.div = function(a, b) return tonumber(a) / tonumber(b) end
O.mod = function(a, b) return tonumber(a) % tonumber(b) end
O.pow = function(b, a) return tonumber(a) ^ tonumber(b) end
O.cat = function(a, b) return a .. b end
O.neg = function(a)    return (-1 * tonumber(a)) end
O.len = function(a)    return #a end
O.isEven = function(a) return (tonumber(a)%2 == 0) end
O.isOdd = function(a)  return (tonumber(a)%2 ~= 0) end
O.isTrue = function(a) if a then return true else return false end end

function H.getOp(op)
	local result
	if 	op == ">" then 
		result = O.gt
	elseif op == "<" then
		result = O.lt
	elseif op == "=" then
		result = O.eq
	elseif op == ">:" then
		result = O.ge
	elseif op == "<:" then
		result = O.le
	elseif op == "~:" then
		result = O.ne
	elseif op == "&" then
		result = O.AND
	elseif op == "|" then
		result = O.OR
	elseif op == "~" then
		result = O.NOT
	elseif op == "|." then
		result = O.XOR
	elseif op == "*" then
		result = O.mul
	elseif op == "+" then
		result = O.add
	elseif op == "-" then
		result = O.sub
	elseif op == "%" then
		result = O.div
	elseif op == "%." then
		result = O.mod
	elseif op == "^" then
		result = O.pow
	elseif op == "," then
		result = O.cat
	elseif op == "-." then
		result = O.neg
	elseif op == "#." then
		result = O.len
	elseif op == "?." then
		result = O.isTrue
	elseif op == "::" then
		result = O.isEven
	elseif op == ".:" then
		result = O.isOdd
	else 
		result = op
	end
	return result
end

H.killSession = function()
   alive = false
  -- goto Quit
end;

I = {}
I.sets = function(tos, nos) return (tos.dress == nos.dress) and (tos.dress == "s") end;
	 -- Sets !!Check both Tables too!!
I.atoms = function(tos, nos) return (type(tos) ~= "table") and (type(nos) ~= "table") end;
	-- Check if TOS A.resetRowsToColumns & NOS are atomic
I.atom = function(it) return not(type(it) == "table") end;

V.dot = function(...)
	local v1, v2 = ...
	local result = 0
	for i = 1, #v1 do
		result = result + v1[i]*v2[i]
	end
	return result
end

V.cross = function(...)
	local v1, v2 = ...
	local result = {}
	for ii = 1, #v1 do
	end	
	return io.write("Not Implemented")
end

A.add = function(...)
	local v1, v2 = ...
	local result = {}
	if #v1 == #v2 then
		if type(v1[1]) ~= 'table' then
			for ii = 1, #v1 do
				result[ii] = v1[ii] + v2[ii]
			end
		else 
			for ii = 1, #v1 do 
				result[ii] = A.add(v1[ii], v2[ii])
			end
		end
		return result
	else 
		return io.write("Error: dimensions incompatibles for attempted operation")	
	end
end

A.multM = function(...)  -- a[j][k] = SUM(i:1:n) b[j][i]*c[i][k]
	local t1, t2 = ...
	local result = {}
	if #t1[1] ~= #t2 then
		return io.write("Error: Incompatible Array Sizes")
	else
		n = #t2
		--  temp = A.transpose(t2)
		for j = 1, n do
			result[j] = {}
			for k = 1, n do
				result[j][k] = 0
				for l = 1, #t2[1] do
					result[j][k] = result[j][k] + t1[j][l] * t2[l][k]  -- dotprod??
				end --for l
			end -- for k
		end -- for j
		return result
	end -- if
end -- fn

A.newSquareM = function(...)
	local dim, defval, idx = ...
	local result = {}
	for ix = 1, dim do
		result[ix] = V.newSameValue(defval, dim)
		--if idx then 
			--result[ix][ix] = 1
		--end --if
	end --for
	return result
end

A.transpose = function(...) -- WRONG!!!!
	local Mtx = ...
--	local MxRow1 = Mtx[1]
	local Columns = #Mtx
	local newRow = {}
	local MxSwitched = {}
--	for i = 1, #Mtx do
	for i = 1, #Mtx[1] do
		MxSwitched[i] = {}
		for j = 1, #Mtx do
			MxSwitched[i][j] = Mtx[j][i]
		end
	end
	return MxSwitched
end

A.rToC = function(...)
	local Mtx = ...
--	local MxRow1 = Mx[1]
	local columns = #Mtx
	local MxSwitched = {}
--	for i = 1, #Mx do
	for ii = 1, #Mtx[1] do
		MxSwitched[ii] = V.newSameValue(0,columns)
	end
	for ii = 1, #Mtx do
		for jj = 1, #Mtx[1] do
			MxSwitched[jj][ii] = Mtx[ii][jj]
		end
	end
	return MxSwitched
end -- ]] --

A.identityMatrix = function (...)
	local ix = ...
	local mx = A.newSuareM(ix)
	for ii = 1, ix do for jj = 1, ix do if ii == jj then mx[ii][jj] = 1 end end end
	return mx
end

A.both = function(tos, nos) return (type(tos) == "table" and type(tos[1]) == "table") and (type(nos[1]) == "table") end
A.matchMult = function(t1, t2) return (#t1[1] == #t2) end
A.sameSize = function(t1, t2) return (#t1 == #t2) and (#t1[1] == #t2[1]) end
V.sameSize = function(t1, t2) return (#t1 == #t2) end

V.reduce = function(...)
	local op, vector, ident = ...      -- could be used to implement sigma, ...
	
	if op == '+' then
		result = ident
		for ii = 1, #vector do
			if type(vector[ii]) ~= table then
				result = result + tonumber(vector[ii])
			else
				result = result + V.sigma(vector[ii])
			end --if
		end --for ii
	elseif op == '*' then
		result = ident
		for ii = 1, #vector do
			if type(vector[ii]) ~= table then
				result = result * tonumber(vector[ii])
			else
				result = result * V.sigma(vector[ii])
			end --if
		end --for ii
	else
		return io.write("ERROR: Operation Not Defined!\n")
	end -- of return lfunc.reduce
end

V.sigma = function(v) 
	local res = 0; 
	for i = 1, #v do 
		res = res + tonumber(v[i]); 
	end
end

V.MULT = function(v) 
	local res = 1; 
	for i = 1, #v do 
		res = res * tonumber(v[i]); 
	end
end

V.reshape = function(...)         -- need to write recursively! n-dim! get dims as vector!
	local cnt, dims, targetVector = ...  -- reshapes vector into a matrix size (n1 x ... x ni)
	local result = {}
	local d = {}
	local i = {}
	for i = 1, #dims do
		for j = 1, dims[i] do 
			cnt = i*j % #targetVector
			result[i][j] = targetVector[cnt]
		end
	local result = {}
    for ix = 1, rows do
		result[ix] = {}
        for jx = 1, cols do
			result[ix][jx] = t1.popBottom()
		end
	end
	return result
end

V.newSameValue = function(...)
	local value, length = ...
	local vector = {}
	for ii = 1, length do
		vector[ii] = value
	end
	return vector
end

V.print = function(...)
	local vectorToPrint = ...
	io.write("\n[ ")
	for ii = 1, #vectorToPrint do
		if type(vectorToPrint[ii]) ~= "table" then
			io.write(tostring(vectorToPrint[ii])," ")
		else
			local subVector = vectorToPrint[ii]
			V.print(subVector)
		end
          if ii == #vectorToPrint then
		    io.write("]\n")
		end 
	end
end;

I.nums = function(...)
	-- Check if TOS & NOS are Nums
	local x, y = ...
	local numbersTorF = (tonumber(x) and tonumber(y))
	return numbersTorF
end

KnV = function(...)   -- KnV(mess, filename) KnV(mess,key,value)
	local mess, arg1, arg2 = ...
	if mess == "open" then
		tincan.load(arg1)  -- arg1 is "filename"
		Fx[arg1] = arg1  --????
    elseif mess == "set" then
		tincan.set(arg1, arg2)  -- key, value = arg1, arg2
	elseif mess == "close" then    
		tincan.save(arg1)
		Fx[arg1] = nil
	elseif mess == "get" then		-- KnV("get",arg1)
		tincan.get(arg1)  			-- key = arg1
	elseif mess == "save" then 	-- KnV("save",arg1)
		tincan.save(arg1)  			-- fileName = arg1
	else
		return io.write('Error')
	end
end

I.dressAs = function( ... )
	local tbl, dressWithThis =  ...
	if type(tbl) == "table" then
		tbl.dress = dressWithThis
	elseif type(tbl) ~= "table" then
		tbl = I.wrapAsTable(tbl)
		tbl.dress = dressWithThis
		return X:push(tbl)
	end
end;

I.wrapAsTable = function( ... )
	return { ... }
end;

I.wrapAsLuaChunk = function( ... )
	local tbl = { ... }
	tbl.dress = "luacode"
	return tbl
end

t.cons = function(...)  -- WTF !!!!!
	local opBrak, dressCode =  ...  -- maybe LISP like thimgy?
	seq = {}
	local tos = X:pop()
	repeat
		if tos ~= opBrak then
			table.insert(seq, 1, tos)
		end
		tos = X:pop()
	until tos == opBrak;
	if dressCode == 'l' then
		local tc = Tchunk:new(seq,dressCode)
		return X:push(tc)
	else
		seq.dress = dressCode
		return X:push(seq)
	end
end 

t.returnNew = function(...)  -- see above!!!
	local tbl = t.cons(...)
	X:push(tbl)
end;

function reverse2Args(arg1,arg2)   -- might prove quite handy
	return arg2, arg1				-- with the stack issue!!!
end

function reverse3Args(arg1,arg2,arg3)  -- might prove quite handy too
	return arg3, arg2, arg1
end

function noOp()
end

Z = { [0] = "Core Functions Dictionary";
    ['z'] = "io.write(Tally(X.data),'\n')";
    ['Z'] = "X:push(Tally(X.data))";
    ['x'] = "X:printStack()";
    ['w'] = "X:swaps()";
    ['u'] = "X:flush()";
    ['t'] = "X:push(A.rToC(X:pop()))";
    ['s'] = "io.write('not implemented yet\n')";
 --	['t'] = "V.reshape(X:pop(),X:pop(),X:pop())";  -- check Moses
   	['p'] = "X:pop()";
    ['k'] = "H.killSession()";
    ['r'] = "X:push(lambda.reduce(reverse3Args(X:pop(),X:pop(),X:pop())))";
 -- ['a'] = "X.push(math.abs(tonumber(X:pop())))";
    ['t'] = "X:push(A.rToC(X:pop()))";
 -- ['s'] = "io.write('not implemented yet\n')";
	['s'] = "X:push(V.reshape(1,X:pop(),X:pop()))";  -- check Moses
    ['k'] = "H.killSession()";
    ['i'] = "X:push(V.iota(X:pop()))";
    ['I'] = "X:push(A.identityMatrix(X:pop()))";
    ['iota'] = "X:push(V.iota0(X:pop()))";
	['d'] = "X:dup()";
	['c'] = "io.write('not implemented yet\n')";
	['b'] = "io.write('not implemented yet\n')";
    ['a'] = "X.push(math.abs(tonumber(X:pop())))";
    
    ["@"] = "X:rot()";
    ["@."] = "X:over()";
    ["@:"] = "X:peek()";   

    ["+"] = "X:push(O.applyArithOps('+', X:pop(), X:pop()))";
    ["+."] = "io.write('not implemented yet\n')";
    ["+:"] = "io.write('not implemented yet\n')";

    ["-"] = "X:push(O.applyArithOps('-', X:pop(), X:pop()))";
    ["-."] = "io.write('not implemented yet\n')";
    ["-:"] = "io.write('not implemented yet\n')";

    ["*"] = "X:push(O.applyArithOps('*', X:pop(), X:pop()))";
    ["*."] = "io.write('not implemented yet\n')";
    ["*:"] = "io.write('not implemented yet\n')";

    ["%"] = "X:push(O.applyArithOps('%', X:pop(), X:pop()))";
    ["%."] = "io.write('not implemented yet\n')";
    ["%:"] = "io.write('not implemented yet\n')";

    ["/"] = "X:dup()";
    ["/."] = "io.write('not implemented yet\n')"; 
    ["/:"] = "X:dups()";

    ["\\"]= "X:drop()";
    ["\\."]= "io.write('not implemented yet\n')"; 
    ["\\:"]= "X:flush()";

    ["_"] = "X:push(math.floor(tonumber(X:pop())))";
    ["_."] = "X:push(Fn.flatten(X:pop(),shallow))";
    ["_:"] = "X:push(Fn.flatten(X:pop()))";

    [";"] = "io.write('not implemented yet\n')";
    [";."] = "io.write('not implemented yet\n')";
    [";:"] = "io.write('not implemented yet\n')";

    ["?"] = "X:push(X:infoTOS())"; 						-- TOS typeQuery;
    ["?."] = "io.write('not implemented yet\n')";
    ["?:"] = "X:push(C.doIf(X:pop(),X:pop(),X:pop()))";	-- check if better to reverse order of args; popped!;
   
    ["#"] = "X:push(Tally(X:peek()))"; 					-- check carefully;
    ["#."] = "io.write('not implemented yet\n')";
    ["#:"] = "X:push(C.loop(X:pop(),X:pop(),X:pop()))";	-- or make two and three arg variants;

    ["$"] = "X:swap()";
    ["$."] = "X:push( X:swaps())";
    ["$:"] = "X:push(C.switch(X:pop(),X:pop()))";		-- switch???;

 -- ["'"] = "io.write('not implemented yet\n')"; 		-- use to quote funName to stack eg. 'hsin;
    ["`"] =  "io.write('not implemented yet\n')"; 		-- comment line!;

    [","] = "X:push(ConcatStrings(X:pop(),X:pop()))";
    [",."] = "io.write('not implemented yet\n')";
    [",:"] = "io.write('not implemented yet\n')";
    
    [".:"] = "io.write('not implemented yet\n')"; 		-- isOdd?;
    ["::"] = "io.write('not implemented yet\n')"; 		-- isEven?;

    ["="] = "X:push(O.eq(X:pop(),X:pop()))";
    ["=."] = "io.write('not implemented yet\n')";
    ["=:"] = "io.write('not implemented yet\n')";

    ["@"] = "X:rot()";
    ["@."] = "X:over()";
    ["@:"] = "X:peek()";

    ["!"]  = "F.facOrExec(X:pop())";
    ["!."] = "io.write('not implemented yet\n')";
    ["!:"] = "io.write('not implemented yet\n')";

    ["^"]  = "X:push(O.applyArithOps('^', X:pop(), X:pop()))";
    ["^."] = "io.write('not implemented yet\n')"; 	-- nth- root maybe?;
    ["^:"] = "X:push(math.log10(X:pop()))"; -- log base 10;

    ["&"]  = "X:push(O.AND(X:pop(),X:pop()))";
    ["&."] = "io.write('not implemented yet\n')";
    ["&:"] = "io.write('not implemented yet\n')";
    
    ["|"]  = "X:push(O.OR(X:pop(),X:pop()))";
    ["|."] = "X:push(O.XOR(X:pop(),X:pop()))";
    ["|:"] = "io.write('not implemented yet\n')";

    ["~"] = "X:push(O.NOT(X:pop()))";
    ["~."] = "io.write('not implemented yet\n')";
    ["~:"] = "X:push(O.ne(X:pop(),X:pop()))";

    ["`."] = "io.write('not implemented yet\n')";
    ["`:"] = "io.write('not implemented yet\n')";

 --   ['\"'] = "String.concatenate(X:pop(),X:pop())";  -- QuotedStringBuilder;
	['"'] = "X:dup()";
	['".'] = "X:drop()";
	['":'] = "io.write('not implemented yet\n')";

 --   ["("] = "io.write('not implemented yet\n')"; -- for vector/table indexing and args;
    ["(."] = "io.write('not implemented yet\n')";
    ["(:"] = "io.write('not implemented yet\n')";

    [")"] = "io.write('not implemented yet\n')";
    [")."] = "io.write('not implemented yet\n')";
    ["):"] = "io.write('not implemented yet\n')";

    ["{"] = "X:push('{'); LFlag = LFlag + 1;"; -- not bool but integer default 0, nested by count?
    ["{."] = "io.write('file read? - not implemented yet\n')";
    ["{:"] = "io.read()";  -- will be reassigned

 --   ["}"] = "t.cons('{', 'l')";  -- impo time to write lambda buildin function!
    ["}"] = "t.cons('{', 'lambda'); LFlag = LFlag - 1"; 
    ["}."] = "io.write('file write? - not implemented yet\n')";
    ["}:"] = "X:printTOS()"; -- will be reassigned

    ["["] = "X:push('[')";
    ["[."] = "io.write('not implemented yet\n')";
    ["[:"] = "io.write('not implemented yet\n')";

    ["]"] = "t.cons('[', 'v')"; 
    ["]."] = "io.write('not implemented yet\n')";
    ["]:"] = "io.write('not implemented yet\n')";
    
    ["<"]  = "X:push(O.lt(X:pop(),X:pop()))";
    ["<."] = "X:push(math.min(X:pop()))"; 			 -- min?;
    ["<:"] = "X:push(O.lbe(X:pop(),X:pop()))";

    [">"]  = "X:push(O.gt(X:pop(),X:pop()))";
    [">."] = "X:push(math.max(X:pop()))"; 			 -- max?;
    [">:"] = "X:push(O.ge(X:pop(),X:pop()))";
  --  [">("] = "io.write('close file???')";

    ["1:"] = "X:push(V.newSameValue(1,X:pop()))";
    ["2:"] = "X:push(A.newSquareM(X:pop()))"; -- produces TOSxTOS all zeros Matrix;
    ["3:"] = "io.write('not implemented yet\n')";
    ["4:"] = "io.write('not implemented yet\n')";
    ["5:"] = "io.write('not implemented yet\n')";
    ["6:"] = "io.write('not implemented yet\n')";
    ["7:"] = "io.write('not implemented yet\n')";
    ["8:"] = "io.write('not implemented yet\n')";
    ["9:"] = "io.write('not implemented yet\n')";
    ["0:"] = "X:push(V.newSameValue(0,X:pop()))";

	['exp'] = "math.exp(X:pop())";
	['log'] = "math.log(X:pop())";
	['nip'] = "X:nip()";
	['dip'] = "X:dip()";
	['tuck'] = "X:tuck()";
	['swaps'] = "X:swaps()"; 
	['peekNOS'] = "X:peekNOS()";
	['peekTOS'] = "X:peekTOS()";
	['peekZOS'] = "X:peekZOS()";
	['dups'] = "X:dups()";
	['push2'] = "X:push2()";
	['ceil'] = "X:push(math.ceil(tonumber(X:pop())))";
	['floor'] = "X:push(math.floor(tonumber(X:pop())))";
   	['sin'] = "X:push(math.sin(tonumber(X:pop())))";
   	['sinh'] = "X:push(math.sinh(tonumber(X:pop())))";
	['cos'] = "X:push(math.cos(tonumber(X:pop())))";
    ['tan'] = "X:push(math.tan(tonumber(X:pop()))";
    ['tanh'] = "X:push(math.tanh(tonumber(X:pop())))";
    ['cot'] = "X:push(math.cotangent(tonumber(X:pop())))";
    ['sec'] = "X:push((1/math.cos(tonumber(X:pop()))))";
    ['csc'] = "X:push((1/math.sin(tonumber(X:pop()))))";
    ['asin'] = "X:push(math.asin(tonumber(X:pop())))";
    ['acos'] = "X:push(math.acos(tonumber(X:pop())))";
    ['atan'] = "X:push(math.atan(tonumber(X:pop())))";
    ['deg'] = "X:push(math.deg(tonumber(X:pop())))";
    ['rad'] = "X:push(math.rad(tonumber(X:pop())))";
  	['sqrt'] = "X:push(math.sqrt(tonumber(X:pop())))";
    ['pi'] = "X:push(math.pi)";
    ['huge'] = "X:push(math.huge)";
    ["0'"] = "X:push(math.huge)";  -- rep +infinity!;
  	['modf'] = "X:push2(math.modf(tonumber(X:pop())))";
    ['rand'] = "X:push(math.random())";
    ['rands'] = "X:push(math.random(tonumber(X:pop()),tonumber(X:pop())))";
    ["reverse"] = "X:push(Fn.reverse(X:pop()))";
	}

function e(...)   -- eval token!!!
    local o = ...
    if type(tonumber(o)) == "number" then
        X:push(o)
	elseif string.match(o, "%w+%'") then
		X:push(string.match(o, "[^%']+"))
    elseif Z[o] then
        assert(loadstring(Z[o]))()
    elseif string.match(o, "[%u%l]+%.") then  -- nu, WTF ????
        local index = string.match(o, "[^%.]+")
        X:push(W[index])
    elseif string.match(o, "[%u%l]+%:") then
        local index = string.match(o, "[^%:]+")
        local var = X:pop()
        if type(var) == "table" then
			W[index] = {}
			W[index] = var
		else
			W[index] = var
		end
    else
        X:push(o)
    end
 end

function getParenSub(...)
	io.write('not implemented yet\n')
end

function P(c)
	if string.match(c,"`%.*") then
		c = " "
	elseif string.match(c,"%\"%b()") then
		local quote = string.match(c,"%b()")
		local tail = string.len(quote) -1
		local str = string.sub(quote,2,string.len(quote)-1)
		Y:pushLast(str)
	--	local rest = string.sub(c, string.len(quote))
--	elseif string.match(c,"[%p%a]%b()") then
		--getParenSub(c)
	else --if rest then
		for NextWord in string.gmatch(c, "%S+") do -- try this out!
		--[[	if Parens then
				local quote = string.match(c,"%b()")
				local tail = string.len(quote) -1
				local str = string.sub(quote,2,string.len(quote)-1)
				Y:pushLast(str)
			end  --]]
			if type(tonumber(NextWord)) == "number" then
				Y:pushLast(tonumber(NextWord))
			else
				local nxchunk = NextWord
				for nw in string.gmatch(nxchunk, "%w*[%[%]%{%}%(%)%`%~%!%@%#%$%%%^%&%*%-%_%+%\\%|%;%'%<%>%,%?%/]?[%.%:]?[%.%:%']?") do
					if string.match(nw, "%.?%w%]")then			--from here
						if tonumber(string.match(nw, "%w+")) then
							Y:pushLast(tonumber(string.match(nw, "%w+")))
						else
							Y:pushLast( string.match(nw, "%w+"))
						end
						Y:pushLast( "]")
					elseif string.match(nw, "%.?%w%}")then			--from here
						Y:pushLast( string.match(nw, "%w+"))
						Y:pushLast( "}") 
					else									-- to here
						Y:pushLast( tostring(nw))
					end
				end --f 	
			end --if
		end
    end
    E()
end

function E() -- ExecQueue
	repeat
        local o = Y:pop() -- Y:popFirst() -- if using List/Deque
        if o then
			if string.match(o, "%S+") then
                e(o)
			end
		end
	until Y:size() < 1 --end repeat!
end

function REPL()
    while alive do
        nu = string.format("%06d",ln)
        prompt = "T(".. nu ..")> "
        io.write(prompt)
        T[ln] = io.read()
        if alive == false then break end
        P(T[ln])
   --     E() -- Eval/exec Deque - check if it's better here for Program Flow!!!
        ln = ln + 1;
    end
end

do --main \\ entry point!!!
    ln = 1
    alive = true
    print("Welcome to Y!\n")
    REPL()
    --::Quit::
    print("Bye!!!")
end --main



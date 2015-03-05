.PHONY: deps doc clean                                                                                                  

all: deps compile

# 获取OTP项目
deps:
	./rebar get-deps

# 编译相关项目,在编译之前先查看依赖项目是否已经存在                                                                     
compile: deps
	./rebar compile

clean:
	./rebar clean
	@-rm erl_crash.dump -f

# 清楚依赖项目
distclean: clean
	./rebar delete-deps

# 测试所有项目
tests:
	./rebar eunit

dialyzer: compile
	@./rebar dialyzer

edoc:
	@./rebar doc 

release:
	@./rebar generate 

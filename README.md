# bluegene
==========

### 关于依赖

bluegene项目里deps目录下的所有子项目都来自于github,参见rebar.config文件中*deps*配置项.
为了使项目工程代码清晰,deps下的子项目都做了清理工作,只保留代码目录.

### 项目结构


### 单元测试

### 需要注意的事

1. 为了提高编译速度，每次执行make时多忽略掉deps目录下的代码，如果改动deps下的代码，切记修改Makefile让deps参与编译！
2. 


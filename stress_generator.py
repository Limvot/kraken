
func_base = "main"
var_base = "var"

numlines = 20
numfuncs = 5

def genFunc():
    stress_test = open("stress_test.krak",'w')
    for i in range(numfuncs):
        stress_test.write('fun '+func_base+(str(i) if i != 0 else "")+'() : int {\n')
        for j in range(numlines):
            stress_test.write("var " + var_base+str(j)+' : int = (5*4+3-2)+1; \n')
        stress_test.write("return 0;\n")
        stress_test.write('}')
        stress_test.write("\n\n")

    stress_test.close()


genFunc()

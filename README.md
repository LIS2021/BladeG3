# BladeG3

## Compile

Compilation of both the virtual machine and the Blade executables can be done by simply running:

        make

Compilation of the virtual machine can be done by running:

        make pipe

Compilation of the Blade executable can be done by running:

        make run_blade

## Execution

The virtual machine can be run through the `pipe` executable, by calling

        ./pipe [OPTIONS] <file>

Blade is included in the executable and can be activated in the options.  
`<file>` must contain the input code to be executed (and possibly protected with blade).

The execution of only Blade can be done through the `run_blade` executable:
        
        ./run_blade [OPTIONS] <file>

`<file>` must contain the input code to run Blade on.

## Options

The available options for the `pipe` executable are the following:

        OPTIONS:
            
            --blade:                     Enable blade optimization
            --model [uniform|simple]:    Select cost model for evaluation
            --weights [constant|simple]: Select weights model for blade
            -s1.1:                       Enable protection vs Spectre1.1
            -v:                          Enable verbose output
            -t <file>:                   Dumps the trace execution in <file>.trace
            -o <file>:                   Save the processed source code in <file>.out

The available options for the `run_blade` executable are the following:

        OPTIONS:
            
            --weights [constant|simple]: Select weights model for blade
            -s1.1:                       Enable protection vs Spectre1.1
            -o <file>:                   Save the processed source code in <file>.out



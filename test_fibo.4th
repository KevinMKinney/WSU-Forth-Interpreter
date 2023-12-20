: n 19 ;

: fibo 
    dup 
    
    1 <= if 
        /' the first two fibo numbers are 0 and 1. Just delete the 
            zero that was used to enter the if and do nothing else  '/
        drop 
    else 
        drop /' delete the 1 that was used to enter the else. '/
        dup  /' save n '/
        1 - fibo    /' get the fibonacci value for n - 1 '/
         
        swap        /' the result is now on top of 'n', so we swap '/   
        2 - fibo    /' get the fibonacci value for n - 2 '/

        /' we have the fibos for n - 1 and n - 2 at the top of the stack. 
            now we just have to add them together: '/
        +
    ;
;

n fibo

/' expected output
[4181.0]
'/
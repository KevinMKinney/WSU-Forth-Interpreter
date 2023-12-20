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

: enumerate_fibo
    while 
        dup
        fibo 
        swap 1 - 
    ;
;

20 enumerate_fibo

/' expected output 
[6765.0,4181.0,2584.0,1597.0,987.0,610.0,377.0,233.0,144.0,89.0,55.0,34.0,21.0,13.0,8.0,5.0,3.0,2.0,1.0,1.0,0.0]
'/
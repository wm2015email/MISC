-- SNOWBALL-CMDLINE:
-- eda_hdlpack math_csw.vhd numeric_csw.vhd fixed_csw.vhd vector_types_pkg.vhd -top types_broken (Fri Nov 22 13:40:40 2024)

-- SNOWBALL-FILE: math_csw.vhd


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee.math_complex.all;

package math_csw is

  type complex_vector is array (natural range <>) of complex;
  type complex_2vector is array (natural range <>) of complex_vector;
  type complex_3vector is array (natural range <>) of complex_2vector;
  type complex_4vector is array (natural range <>) of complex_3vector;
  
  type real_2vector is array (natural range <>) of real_vector;
  type real_3vector is array (natural range <>) of real_2vector;
  type real_4vector is array (natural range <>) of real_3vector;
  
  type integer_2vector is array (natural range <>) of integer_vector;
  type integer_3vector is array (natural range <>) of integer_2vector;
  type integer_4vector is array (natural range <>) of integer_3vector;

  type string_vector is array (natural range <>) of string;

  function "ABS" (val : complex_vector) return real_vector;
  function "ABS" (val : complex_2vector) return real_2vector;
  function "ABS" (val : complex_3vector) return real_3vector;

  function "-" (vec : integer_vector) return integer_vector;
  function "+" (a,b : integer_vector) return integer_vector;
  function "-" (a,b : integer_vector) return integer_vector;
  function "+" (vec : integer_vector; off : integer) return integer_vector;
  function "-" (vec : integer_vector; off : integer) return integer_vector;
  function "*" (vec : integer_vector; scl : integer) return integer_vector;
  function "*" (scl : integer; vec : integer_vector) return integer_vector;
  function "/" (vec : integer_vector; div : integer) return integer_vector;
  function "mod" (vec : integer_vector; div : integer) return integer_vector;
  function "*" (a,b : integer_vector) return integer_vector;                 
  function "/" (a,b : integer_vector) return integer_vector;                 

  function sum (vec : integer_vector) return integer;
  function prod (vec : integer_vector) return integer;

  function "-" (vec : real_vector) return real_vector;
  function "+" (a,b : real_vector) return real_vector;
  function "-" (a,b : real_vector) return real_vector;
  function "+" (vec : real_vector; off : real) return real_vector;
  function "-" (vec : real_vector; off : real) return real_vector;
  function "*" (vec : real_vector; scl : real) return real_vector;
  function "*" (scl : real; vec : real_vector) return real_vector;
  function "/" (vec : real_vector; div : real) return real_vector;
  function "*" (a,b : real_vector) return real_vector;                 
  function "/" (a,b : real_vector) return real_vector;                 

  function sum (vec : real_vector) return real;
  function prod (vec : real_vector) return real;
  
  function conj (vec : complex_vector) return complex_vector;
  function "-" (vec : complex_vector) return complex_vector;
  function "+" (a,b : complex_vector) return complex_vector;
  function "-" (a,b : complex_vector) return complex_vector;
  function "+" (vec : complex_vector; off : real) return complex_vector;
  function "+" (vec : complex_vector; off : complex) return complex_vector;
  function "-" (vec : complex_vector; off : real) return complex_vector;
  function "-" (vec : complex_vector; off : complex) return complex_vector;
  function "*" (vec : complex_vector; scl : real) return complex_vector;
  function "*" (vec : complex_vector; scl : complex) return complex_vector;
  function "*" (scl : real; vec : complex_vector) return complex_vector;
  function "*" (scl : complex; vec : complex_vector) return complex_vector;
  function "/" (vec : complex_vector; div : real) return complex_vector;
  function "/" (vec : complex_vector; div : complex) return complex_vector;

  function ceillog2 (num: natural) return natural;
  function ceillog2 (num: natural; min_result: natural; max_result: natural := 31) return natural;
  function ceillog2 (vec: integer_vector) return integer_vector;

  function ceillog3 (num: natural) return natural;
  function ceillog3 (num: natural; min_result: natural) return natural;

  function ceillogn (num: natural; base: natural) return natural;
  function ceillogn (num: natural; base: natural; min_result: natural) return natural;

  function floorlog2 (num: natural) return natural;

  function floorlogn (num: natural; base: natural; min_result: natural := 0) return natural;

  function next_pow2 (greater_than_or_equal_to: natural) return natural;

  function is_pow2 (num: natural) return boolean;

  function ceildiv (numerator: integer; denominator: integer) return integer;

  function floordiv (numerator: integer; denominator: integer) return integer;

  function prev_mult_of(factor: positive; less_than_or_equal_to: natural) return natural;

  function next_mult_of(factor: positive; greater_than_or_equal_to: natural) return natural;

  function round (num: real) return integer;

  function ceil (num: real) return integer;

  function floor (num: real) return integer;

  function float_equal (a,b : real; tol : real := 1.0e-6) return boolean;
  function float_equal (a,b : real_vector; tol : real := 1.0e-6) return boolean;

  function iseven (num: integer) return boolean;

  function maximum(vec : integer_vector; val : integer) return integer_vector;
  function maximum(val : integer; vec : integer_vector) return integer_vector;
  function minimum(vec : integer_vector; val : integer) return integer_vector;
  function minimum(val : integer; vec : integer_vector) return integer_vector;

  function factorial (n: natural) return natural;

  function nchoosek (n: natural; k: natural) return natural;

  function factorial_r (n: natural) return real;

  function nchoosek_r (n: natural; k: natural) return real;

  function gcd(a, b: integer) return integer;

  function lcm(a, b: integer) return integer;

  function divides(a, b: integer) return boolean;

  function sinc(val : real) return real;

  function "*"(a : real; b : std_logic) return real;
  function "*"(a : std_logic; b : real) return real;
  function "*"(a : integer; b : std_logic) return integer;
  function "*"(a : std_logic; b : integer) return integer;

  function is_equal (a,b : integer ) return std_logic ;
  function is_equal (a,b : integer ) return natural ;
  function is_equal (a,b : std_logic ) return natural ;

  function "+"(a : integer; b : boolean) return integer;
  function "+"(a : boolean; b : integer) return integer;
  function "-"(a : integer; b : boolean) return integer;

  function "*"(a : integer; b : boolean) return integer;
  function "*"(a : boolean; b : integer) return integer;
  function "*"(a : real; b : boolean) return real;
  function "*"(a : boolean; b : real) return real;

  function to_string (int_vec : integer_vector) return string;

  function invert_index_map (map_vector : integer_vector) return integer_vector;

end math_csw;

package body math_csw is

  function "ABS" (val : complex_vector) return real_vector
  is
    variable result : real_vector(val'range);
  begin
    for ii in val'range loop
      result(ii) := abs val(ii);
    end loop;
    return result;
  end function "ABS";

  function "ABS" (val : complex_2vector) return real_2vector
  is
    variable result : real_2vector(val'range)(val'element'range);
  begin
    for ii in val'range loop
      result(ii) := abs val(ii);
    end loop;
    return result;
  end function "ABS";

  function "ABS" (val : complex_3vector) return real_3vector
  is
    variable result : real_3vector(val'range)(val'element'range)(val'element'element'range);
  begin
    for ii in val'range loop
      result(ii) := abs val(ii);
    end loop;
    return result;
  end function "ABS";

  function "-" (vec : integer_vector) return integer_vector
  is
    variable result : integer_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := -vec(ii);
    end loop;
    return result;
  end function "-";

  function "+" (a,b : integer_vector) return integer_vector
  is
    variable result : integer_vector(a'range);
  begin
    assert a'left = b'left and a'right = b'right and a'ascending = b'ascending
      report "Expected identical ranges."
      severity failure;
    
    for ii in result'range loop
      result(ii) := a(ii) + b(ii);
    end loop;
    return result;
  end function "+";

  function "-" (a,b : integer_vector) return integer_vector
  is
    variable result : integer_vector(a'range);
  begin
    assert a'left = b'left and a'right = b'right and a'ascending = b'ascending
      report "Expected identical ranges."
      severity failure;
    
    for ii in result'range loop
      result(ii) := a(ii) - b(ii);
    end loop;
    return result;
  end function "-";

  function "+" (vec : integer_vector; off : integer) return integer_vector
  is
    variable result : integer_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) + off;
    end loop;
    return result;
  end function "+";

  function "-" (vec : integer_vector; off : integer) return integer_vector
  is
    variable result : integer_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) - off;
    end loop;
    return result;
  end function "-";

  function "*" (vec : integer_vector; scl : integer) return integer_vector
  is
    variable result : integer_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) * scl;
    end loop;
    return result;
  end function "*";

  function "*" (scl : integer; vec : integer_vector) return integer_vector is
  begin
    return vec * scl;
  end function "*";

  function "/" (vec : integer_vector; div : integer) return integer_vector
  is
    variable result : integer_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) / div;
    end loop;
    return result;
  end function "/";

  function "mod" (vec : integer_vector; div : integer) return integer_vector
  is
    variable result : integer_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) mod div;
    end loop;
    return result;
  end function "mod";

  function "*" (a,b : integer_vector) return integer_vector is
    variable a_rng : integer_vector(0 to a'length-1) := a;
    variable b_rng : integer_vector(0 to b'length-1) := b;
    variable result : integer_vector(0 to a'length-1);
  begin
    assert a'length=b'length
      report "Vectors must have the same length."
      severity failure;
    for ii in result'range loop
      result(ii) := a_rng(ii) * b_rng(ii);
    end loop;
    return result;
  end function "*";

  function "/" (a,b : integer_vector) return integer_vector
  is
    variable a_rng : integer_vector(0 to a'length-1) := a;
    variable b_rng : integer_vector(0 to b'length-1) := b;
    variable result : integer_vector(0 to a'length-1);
  begin
    assert a'length=b'length
      report "Vectors must have the same length."
      severity failure;
    for ii in result'range loop
      result(ii) := a_rng(ii) / b_rng(ii);
    end loop;
    return result;
  end function "/";

  function sum (vec : integer_vector) return integer is
    variable result : integer := 0;
  begin
    for ii in vec'range loop
      result := result + vec(ii);
    end loop;
    return result;
  end function sum;
                                          
  function prod (vec : integer_vector) return integer is
    variable result : integer := 1;
  begin
    for ii in vec'range loop
      result := result * vec(ii);
    end loop;
    return result;
  end function prod;

  function "-" (vec : real_vector) return real_vector
  is
    variable result : real_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := -vec(ii);
    end loop;
    return result;
  end function "-";

  function "+" (a,b : real_vector) return real_vector
  is
    variable result : real_vector(a'range);
  begin
    assert a'left = b'left and a'right = b'right and a'ascending = b'ascending
      report "Expected identical ranges."
      severity failure;
    
    for ii in result'range loop
      result(ii) := a(ii) + b(ii);
    end loop;
    return result;
  end function "+";

  function "-" (a,b : real_vector) return real_vector
  is
    variable result : real_vector(a'range);
  begin
    assert a'left = b'left and a'right = b'right and a'ascending = b'ascending
      report "Expected identical ranges."
      severity failure;
    
    for ii in result'range loop
      result(ii) := a(ii) - b(ii);
    end loop;
    return result;
  end function "-";

  function "+" (vec : real_vector; off : real) return real_vector
  is
    variable result : real_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) + off;
    end loop;
    return result;
  end function "+";

  function "-" (vec : real_vector; off : real) return real_vector
  is
    variable result : real_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) - off;
    end loop;
    return result;
  end function "-";

  function "*" (vec : real_vector; scl : real) return real_vector
  is
    variable result : real_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) * scl;
    end loop;
    return result;
  end function "*";

  function "*" (scl : real; vec : real_vector) return real_vector is
  begin
    return vec * scl;
  end function "*";

  function "/" (vec : real_vector; div : real) return real_vector
  is
    variable result : real_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) / div;
    end loop;
    return result;
  end function "/";

  function "*" (a,b : real_vector) return real_vector is
    variable a_rng : real_vector(0 to a'length-1) := a;
    variable b_rng : real_vector(0 to b'length-1) := b;
    variable result : real_vector(0 to a'length-1);
  begin
    assert a'length=b'length
      report "Vectors must have the same length."
      severity failure;
    for ii in result'range loop
      result(ii) := a_rng(ii) * b_rng(ii);
    end loop;
    return result;
  end function "*";

  function "/" (a,b : real_vector) return real_vector
  is
    variable a_rng : real_vector(0 to a'length-1) := a;
    variable b_rng : real_vector(0 to b'length-1) := b;
    variable result : real_vector(0 to a'length-1);
  begin
    assert a'length=b'length
      report "Vectors must have the same length."
      severity failure;
    for ii in result'range loop
      result(ii) := a_rng(ii) / b_rng(ii);
    end loop;
    return result;
  end function "/";

  function sum (vec : real_vector) return real is
    variable result : real := 0.0;
  begin
    for ii in vec'range loop
      result := result + vec(ii);
    end loop;
    return result;
  end function sum;
                                          
  function prod (vec : real_vector) return real is
    variable result : real := 1.0;
  begin
    for ii in vec'range loop
      result := result * vec(ii);
    end loop;
    return result;
  end function prod;

  function conj (vec : complex_vector) return complex_vector is
    variable result : complex_vector(vec'range);
  begin
    for ii in result'range loop
      result(ii) := conj(vec(ii));
    end loop;
    return result;
  end function conj;

  function "-" (vec : complex_vector) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := -vec(ii);
    end loop;
    return result;
  end function "-";

  function "+" (a,b : complex_vector) return complex_vector
  is
    variable result : complex_vector(a'range);
  begin
    assert a'left = b'left and a'right = b'right and a'ascending = b'ascending
      report "Expected identical ranges."
      severity failure;
    
    for ii in result'range loop
      result(ii) := a(ii) + b(ii);
    end loop;
    return result;
  end function "+";

  function "-" (a,b : complex_vector) return complex_vector
  is
    variable result : complex_vector(a'range);
  begin
    assert a'left = b'left and a'right = b'right and a'ascending = b'ascending
      report "Expected identical ranges."
      severity failure;
    
    for ii in result'range loop
      result(ii) := a(ii) - b(ii);
    end loop;
    return result;
  end function "-";

  function "+" (vec : complex_vector; off : real) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) + off;
    end loop;
    return result;
  end function "+";

  function "+" (vec : complex_vector; off : complex) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) + off;
    end loop;
    return result;
  end function "+";

  function "-" (vec : complex_vector; off : real) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) - off;
    end loop;
    return result;
  end function "-";

  function "-" (vec : complex_vector; off : complex) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) - off;
    end loop;
    return result;
  end function "-";

  function "*" (vec : complex_vector; scl : real) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) * scl;
    end loop;
    return result;
  end function "*";

  function "*" (vec : complex_vector; scl : complex) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) * scl;
    end loop;
    return result;
  end function "*";

  function "*" (scl : real; vec : complex_vector) return complex_vector is
  begin
    return vec * scl;
  end function "*";

  function "*" (scl : complex; vec : complex_vector) return complex_vector is
  begin
    return vec * scl;
  end function "*";

  function "/" (vec : complex_vector; div : real) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) / div;
    end loop;
    return result;
  end function "/";

  function "/" (vec : complex_vector; div : complex) return complex_vector
  is
    variable result : complex_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := vec(ii) / div;
    end loop;
    return result;
  end function "/";

  function ceillog2 (
    num: natural
  ) return natural is
    variable currpow : natural;
  begin
    return ceillog2(num => num, min_result => 0);
  end ceillog2;

  function ceillog2 (
    num        : natural;
    min_result : natural;
    max_result : natural := 31                                              
  ) return natural is
    variable currpow : natural;
  begin
                               
    currpow := min_result;

    while (2**currpow < num) and (currpow < max_result) loop
      currpow := currpow + 1;
    end loop;

    return currpow;
  end ceillog2;

  function ceillog2 (vec : integer_vector) return integer_vector is
    variable result : integer_vector(vec'range);
  begin
    for xx in vec'range loop
      result(xx) := ceillog2(vec(xx));
    end loop;
           
    return result;
  end ceillog2;
  
  function ceillog3 (
    num: natural
  ) return natural is
  begin
    return ceillogn(num, 3, 0);    
  end ceillog3;
  
  function ceillog3 (
    num: natural;
    min_result : natural
  ) return natural is
  begin
    return ceillogn(num, 3, min_result);    
  end ceillog3;  

  function ceillogn (
    num: natural;
    base: natural
  ) return natural is
    variable currpow : natural;
  begin
    return ceillogn(num, base, 0);
  end ceillogn;

  function ceillogn (
    num: natural;
    base: natural;
    min_result : natural
  ) return natural is
    variable currpow : natural;
  begin
                               
    currpow := min_result;

    while base**currpow < num loop
      currpow := currpow + 1;
    end loop;

    return currpow;
  end ceillogn;

  function floorlog2 (num: natural) return natural is
    variable currpow : natural := 0;
  begin
                                     
    while 2**(currpow+1) <= num loop
      currpow := currpow + 1;
    end loop;

    return currpow;
  end function floorlog2;
    
  function floorlogn (
    num        : natural;
    base       : natural;
    min_result : natural := 0
  ) return natural is
    variable currpow    : natural;
  begin
                               
    currpow := min_result;

    while base**(currpow+1) <= num loop
      currpow := currpow + 1;
    end loop;

    return currpow;
  end function floorlogn;

  function next_pow2 (
    greater_than_or_equal_to: natural
  ) return natural is
    variable currpow : natural;
  begin
                               
    currpow := 1;

    while currpow < greater_than_or_equal_to loop
      currpow := currpow*2;
    end loop;

    return currpow;
  end next_pow2;

  function is_pow2 (num: natural) return boolean is
  begin
    return 2**ceillog2(num,0) = num;
  end is_pow2;

  function ceildiv (
    numerator: integer;
    denominator: integer
  ) return integer is
    variable result : integer;
  begin
    assert denominator /= 0 report "ceildiv division by zero" severity failure;
    result := numerator / denominator;
    if denominator * result < numerator then
      result := result + 1;
    end if;
    assert denominator * result >= numerator report "ceildiv got wrong answer" severity failure;
    return result;
  end ceildiv;

  function floordiv (
    numerator: integer;
    denominator: integer
  ) return integer is
    variable result : integer;
  begin
    assert denominator /= 0 report "floordiv division by zero" severity failure;
    result := numerator / denominator;
    if denominator * result > numerator then
      result := result - 1;
    end if;
    assert denominator * result <= numerator report "floordiv got wrong answer. num is " & integer'image(numerator) & " den*result is " & integer'image(denominator * result) severity failure;
    return result;
  end floordiv;

  function prev_mult_of(
    factor: positive;
    less_than_or_equal_to: natural
  ) return natural is
  begin
    return factor * (less_than_or_equal_to / factor);
  end function prev_mult_of;

  function next_mult_of(
    factor: positive;
    greater_than_or_equal_to: natural
  ) return natural is
  begin
    if 1 = factor then
      return greater_than_or_equal_to;
    else
      return factor * ((greater_than_or_equal_to + factor - 1) / factor);
    end if;
  end function next_mult_of;

  function round (
    num : real)
    return integer is
  begin
    return floor(num + 0.5);
  end function round;

  function ceil (
    num: real
  ) return integer is
    variable result : integer;
  begin
    result := integer(num);
    if real(result) < num then
      result := result + 1; 
    end if;
    return result;
  end ceil;

  function floor (
    num: real
  ) return integer is
    variable result : integer;
  begin
    result := integer(num);
    if real(result) > num then
      result := result - 1; 
    end if;
    return result;
  end floor;

  function float_equal (a,b : real; tol : real := 1.0e-6) return boolean is
    variable aa, bb : real;
  begin
    assert tol >= 0.0 report "Tolerance must be non-negative." severity failure;

    if abs(b) > abs(a) then
      aa := b; bb := a;
    else
      aa := a; bb := b;
    end if;

    if 0.0 = bb then
                                                                  
      return abs(aa - bb) < tol;
    else
                                    
      return abs((aa - bb) / aa) < tol;
    end if;
  end function float_equal;

  function float_equal (a,b : real_vector; tol : real := 1.0e-6) return boolean is
    alias a_up : real_vector(0 to a'length-1) is a;
    alias b_up : real_vector(0 to b'length-1) is b;
  begin
                                                  
    if a'length /= b'length then
      return false;
    end if;

    for ii in a_up'range loop
      if not float_equal(a_up(ii), b_up(ii)) then
        return false;
      end if;
    end loop;
    return true;
  end function float_equal;

  function iseven (
    num: integer
  ) return boolean is
  begin
    return (num mod 2) = 0;
  end iseven;

  function maximum(vec : integer_vector; val : integer) return integer_vector is
    variable result : integer_vector(vec'range);
  begin
    for ii in vec'range loop
      result(ii) := maximum(val, vec(ii));
    end loop;
    return result;
  end function maximum;
  
  function maximum(val : integer; vec : integer_vector) return integer_vector is
  begin
    return maximum(vec, val);
  end function maximum;
  
  function minimum(vec : integer_vector; val : integer) return integer_vector is
    variable result : integer_vector(vec'range);
  begin
    for ii  in vec'range loop
      result(ii) := minimum(val, vec(ii));
    end loop;
    return result;
  end function minimum;
  
  function minimum(val : integer; vec : integer_vector) return integer_vector is
  begin
    return minimum(vec, val);
  end function minimum;

  function factorial (
    n: natural
  ) return natural is
  begin
    if n > 1 then
      return n * factorial(n-1);
    else
      return 1;
    end if;
  end function factorial;

  function nchoosek (
    n: natural;
    k: natural
  ) return natural is
    variable curr_prod : natural;
    variable kprime : natural;
  begin
    if k > n then
      return 0;
    else

      if k > n/2 then
        kprime := n-k;
      else
        kprime := k;
      end if;

      curr_prod := 1;
      for ii in 1 to kprime loop
        curr_prod := (curr_prod *  (n-kprime+ii)) / ii;
      end loop;

      return curr_prod;
    end if;
  end function nchoosek;

  function factorial_r (
    n: natural
  ) return real is
  begin
    if n > 1 then
      return real(n) * factorial_r(n-1);
    else
      return 1.0;
    end if;
  end function factorial_r;

  function nchoosek_r (
    n: natural;
    k: natural
  ) return real is
    variable curr_prod : real;
    variable kprime : natural;
  begin
    if k > n then
      return 0.0;
    else

      if k > n/2 then
        kprime := n-k;
      else
        kprime := k;
      end if;

      curr_prod := 1.0;
      for ii in 1 to kprime loop
        curr_prod := (curr_prod *  real(n-kprime+ii)) / real(ii);
      end loop;

      return curr_prod;
    end if;
  end function nchoosek_r;

  function gcd(a, b: integer) return integer is
    variable aa, bb, rr : integer;
  begin
                            
    if a >= b then
      aa := a;  bb := b;
    else
      aa := b;  bb := a;
    end if;

    while bb /= 0 loop
      rr := aa mod bb;
      aa := bb;
      bb := rr;
    end loop;

    return aa;
  end function gcd;

  function lcm(a, b: integer) return integer is
  begin
    return (abs (a * b)) / gcd(a, b);
  end function lcm;

  function divides(a, b: integer) return boolean is
  begin
    return 0 = (b mod a);
  end function divides;

  function sinc(val : real) return real is
  begin
    if 0.0 = val then
      return 1.0;
    else
      return sin(math_pi*val) / (math_pi*val);
    end if;
  end function sinc;

  function "*"(a : real; b : std_logic) return real is
  begin
    if '1' = to_x01(b) then
      return a;
    else
      return 0.0;
    end if;
  end function "*";
  function "*"(a : std_logic; b : real) return real is
  begin
    return b * a;
  end function "*";
  function "*"(a : integer; b : std_logic) return integer is
  begin
    if '1' = to_x01(b) then
      return a;
    else
      return 0;
    end if;
  end function "*";
  function "*"(a : std_logic; b : integer) return integer is
  begin
    return b * a;
  end function "*";

  function is_equal (a,b : integer ) return std_logic is 
  begin
    if a = b then
      return '1';
    else
      return '0';
    end if ;
  end function is_equal ;

  function is_equal (a,b : integer ) return natural is 
  begin
    if a = b then
      return 1;
    else
      return 0;
    end if ;
  end function is_equal ;

  function is_equal (a,b : std_logic ) return natural is 
  begin
    if a = b then
      return 1;
    else
      return 0;
    end if ;
  end function is_equal ;

  function "+" (a : integer; b : boolean) return integer is
  begin
    if b then
      return a + 1;
    else
      return a;
    end if;
  end function "+";

  function "+" (a : boolean; b : integer) return integer is
  begin
    return b + a;
  end function "+";

  function "-" (a : integer; b : boolean) return integer is
  begin
    if b then
      return a - 1;
    else
      return a;
    end if;
  end function "-";

  function "*" (a : integer; b : boolean) return integer is
  begin
    if b then
      return a;
    else
      return 0;
    end if;
  end function "*";

  function "*" (a : boolean; b : integer) return integer is
  begin
    return b * a;
  end function "*";

  function "*" (a : real; b : boolean) return real is
  begin
    if b then
      return a;
    else
      return 0.0;
    end if;
  end function "*";

  function "*" (a : boolean; b : real) return real is
  begin
    return b * a;
  end function "*";

  function to_string (int_vec : integer_vector) return string is
                                                                              
    function to_string_elem(int_vec : integer_vector) return string is
    begin
      if int_vec'length = 1 then
        return to_string(int_vec(int_vec'left));
      else
        return to_string(int_vec(int_vec'left)) & ", " & to_string_elem(int_vec(int_vec'left+1 to int_vec'right));
      end if;
    end function to_string_elem;
    
  begin
    return "(" & to_string_elem(int_vec) & ")";
  end function to_string;

  function invert_index_map (map_vector : integer_vector) return integer_vector is
    variable inverted_map : integer_vector(map_vector'range);
  begin
                                                                                
    for ii in map_vector'range loop
      inverted_map(map_vector(ii)) := ii;
    end loop;

    return inverted_map;
  end function invert_index_map;

end package body math_csw;
-- SNOWBALL-FILE: numeric_csw.vhd


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.math_csw.all;
use std.textio.all;

package numeric_csw is

  type resize_num_add_bits is array (1 to 2) of integer;
  type resize_alignment is (align_msb, align_lsb);
  type resize_method is (resize_nosat, resize_trunc, resize_roundne, resize_roundno, resize_roundtz, resize_roundaz);

  function to_integer (bit : std_logic) return integer;

  function to_natural (bit : std_logic) return natural;

  function to_natural (datum : unsigned) return natural;

  function to_integer (bool : boolean) return natural;
                                                                           
  function to_natural (bool : boolean) return natural;

  function to_signed (datum : unsigned) return signed;

  function to_signed (datum : real; width: natural) return signed;

  function to_unsigned (datum : real; width: natural) return unsigned;

  function to_unsigned (bit : std_logic; width: positive := 1) return unsigned;

  function to_unsigned (datum : signed; warn : boolean := true) return unsigned;

  function to_std_logic (bool : boolean) return std_logic;

  function to_std_logic (num : integer) return std_logic;

  function to_std_logic_vector (num : integer; width : natural) return std_logic_vector;

  function to_std_logic_vector (bv : boolean_vector) return std_logic_vector;

  function to_boolean_vector (slv : std_logic_vector) return boolean_vector;

  function value_max (val : signed) return signed;
  function value_max (val : unsigned) return unsigned;

  function value_min_wanted (val : signed) return signed;
  function value_min (val : signed) return signed;
  function value_min (val : unsigned) return unsigned;

  function value_zero (val : signed) return signed;
  function value_zero (val : unsigned) return unsigned;

  function negate (num: signed) return signed;

  function negate (num: unsigned) return signed;

  function remove_max_neg (datum: signed) return signed;

  function rmsgn (num           : signed;
                  warn_overflow : boolean := true;
                  saturate      : boolean := true) return signed;

  function rmsgn (num           : unsigned;
                  warn_overflow : boolean := true;
                  saturate      : boolean := true) return unsigned;

  function wrap_sum (num           : signed;
                     warn_overflow : boolean := false;
                     saturate      : boolean := false) return signed;

  function wrap_sum (num           : unsigned;
                     warn_overflow : boolean := false;
                     saturate      : boolean := false) return unsigned;

  function iszeros (num: std_logic_vector) return boolean;

  function iszeros (num: signed) return boolean;

  function iszeros (num: unsigned) return boolean;

  function add (a: signed; b: std_logic) return signed;
  function add (a: unsigned; b: std_logic) return unsigned;
  function add (a: std_logic; b: signed) return signed;
  function add (a: std_logic; b: unsigned) return unsigned;
  function add (a: integer; b: std_logic) return integer;
  function "+" (a: integer; b: std_logic) return integer;
  function add (a: std_logic; b: integer) return integer;
  function "+" (a: std_logic; b: integer) return integer;

  function diff (a: signed; b: std_logic) return signed;
  function diff (a: unsigned; b: std_logic) return unsigned;
  function diff (a: std_logic; b: signed) return signed;
  function diff (a: std_logic; b: unsigned) return unsigned;
  function diff (a : integer; b : std_logic) return integer;
  function "-" (a  : integer; b : std_logic) return integer;
  function diff (a : std_logic; b : integer) return integer;
  function "-" (a  : std_logic; b : integer) return integer;

  function add (a: unsigned; b: signed) return signed;
  function "+" (a: unsigned; b: signed) return signed;
  function add (a: signed; b: unsigned) return signed;
  function "+" (a: signed; b: unsigned) return signed;

  function diff (a: unsigned; b: signed) return signed;
  function "-" (a: unsigned; b: signed) return signed;
  function diff (a: signed; b: unsigned) return signed;
  function "-" (a: signed; b: unsigned) return signed;

  function prod (a: signed; b: unsigned) return signed;
  function "*"  (a: signed; b: unsigned) return signed;  
  function prod (a: unsigned; b: signed) return signed;
  function "*"  (a: unsigned; b: signed) return signed;  

  function prod (a: signed; b: std_logic) return signed;
  function "*"  (a: signed; b: std_logic) return signed;  
  function prod (a: std_logic; b: signed) return signed;
  function "*"  (a: std_logic; b: signed) return signed;  

  function prod (a: unsigned; b: std_logic) return unsigned;
  function "*"  (a: unsigned; b: std_logic) return unsigned;  
  function prod (a: std_logic; b: unsigned) return unsigned;
  function "*"  (a: std_logic; b: unsigned) return unsigned;  

  function prod (a: std_logic_vector; b: std_logic) return std_logic_vector;
  function "*"  (a: std_logic_vector; b: std_logic) return std_logic_vector;  
  function prod (b: std_logic; a: std_logic_vector) return std_logic_vector;
  function "*"  (b: std_logic; a: std_logic_vector) return std_logic_vector;  

  function ">" (l : unsigned; r : signed) return boolean;
  function ">" (l : signed; r : unsigned) return boolean;
  function "<" (l : unsigned; r : signed) return boolean;
  function "<" (l : signed; r : unsigned) return boolean;

  function vector_or (vec: std_logic_vector) return std_logic;
  function vector_or (vec: unsigned) return std_logic;
  function vector_or (vec: signed) return std_logic;

  function vector_and (vec: std_logic_vector) return std_logic;
  function vector_and (vec: unsigned) return std_logic;
  function vector_and (vec: signed) return std_logic;

  function vector_xor (vec : std_logic_vector) return std_logic;
  function vector_xor (vec : unsigned) return std_logic;
  function vector_xor (vec : signed) return std_logic;

  function shift_left_logical (vec : std_logic_vector; nbits : natural) return std_logic_vector;  

  function shift_right_logical (vec : std_logic_vector; nbits : natural) return std_logic_vector;  

  function circle_rotate_left (vec : std_logic_vector; nbits : integer) return std_logic_vector;

  function circle_rotate_right (vec : std_logic_vector; nbits : integer) return std_logic_vector;  

  function logical_and (l : std_logic; r : std_logic_vector) return std_logic_vector;  
                                                               
  function logical_and  (l : std_logic_vector; r : std_logic) return std_logic_vector;  

  function exclusive_or (l : std_logic; r : std_logic_vector) return std_logic_vector;  

  function exclusive_or (l : std_logic_vector; r : std_logic) return std_logic_vector;  

  function logical_and (l : std_ulogic; r : boolean) return std_logic;
  function logical_and (l : boolean; r : std_ulogic) return std_logic;
  function logical_or (l  : std_ulogic; r : boolean) return std_logic;
  function logical_or (l  : boolean; r : std_ulogic) return std_logic;
  function "and" (l : std_ulogic; r : boolean) return std_logic;
  function "and" (l : boolean; r : std_ulogic) return std_logic;
  function "or" (l  : std_ulogic; r : boolean) return std_logic;
  function "or" (l  : boolean; r : std_ulogic) return std_logic;

  function to_01_bitwise(vec : std_logic_vector; xmap : std_logic := '0') return std_logic_vector;
  function to_01_bitwise(vec : unsigned; xmap : std_logic := '0') return unsigned;
  function to_01_bitwise(vec : signed; xmap : std_logic := '0') return signed;

  function to_01_bw(vec : std_logic_vector; xmap : std_logic := '0') return std_logic_vector;
  function to_01_bw(vec : unsigned; xmap : std_logic := '0') return unsigned;
  function to_01_bw(vec : signed; xmap : std_logic := '0') return signed;
  
  function count_ones(vec : std_logic_vector) return natural;

  function byte_reverse (sig : std_logic_vector) return std_logic_vector;
  function bit_reverse (sig : std_logic_vector) return std_logic_vector;

  function find_rightmost(vec : std_logic_vector; val : std_ulogic) return integer;
  function find_leftmost(vec  : std_logic_vector; val : std_ulogic) return integer;

  function from_hstring(str : string; len : natural) return std_logic_vector;
  function from_hstring(str : string; len : natural) return unsigned;
  function from_hstring(str : string; len : natural) return signed;

  function from_ostring (str : string; len : natural) return std_logic_vector;
  function from_ostring (str : string; len : natural) return unsigned;
  function from_ostring (str : string; len : natural) return signed;

  function from_bstring (str : string) return std_logic_vector;
  function from_bstring (str : string) return unsigned;
  function from_bstring (str : string) return signed;

  function from_dstring (str : string; len : natural) return std_logic_vector;
  function from_dstring (str : string; len : natural) return unsigned;
  function from_dstring (str : string; len : natural) return signed;

  procedure dec_write(variable ln : inout line; val : std_logic);
  procedure dec_write(variable ln : inout line; val : signed);
  procedure dec_write(variable ln : inout line; val : unsigned);
  procedure dec_write(variable ln : inout line; val : std_logic_vector);
  alias dwrite is dec_write [ line, std_logic ];
  alias dwrite is dec_write [ line, signed ];
  alias dwrite is dec_write [ line, unsigned ];
  alias dwrite is dec_write [ line, std_logic_vector ];

  function resize (datum: std_logic; length: integer) return std_logic_vector;
  function resize (datum: signed; length: integer; align: resize_alignment; method: resize_method := resize_nosat) return signed;
  function resize (datum: unsigned; length: integer; align: resize_alignment; method: resize_method := resize_nosat) return unsigned;
  function resize (datum: std_logic_vector; length: integer; align: resize_alignment) return std_logic_vector;
  function resize (datum: signed; addb: resize_num_add_bits; method: resize_method := resize_nosat) return signed;
  function resize (datum: unsigned; addb: resize_num_add_bits; method: resize_method := resize_nosat) return unsigned;
  function resize (datum: std_logic_vector; addb: resize_num_add_bits) return std_logic_vector;
  function resize_pre_rnd (datum: signed; length: natural) return signed;
  function resize_fin_rnd (datum: signed) return signed;

  function sign_extend (datum : signed; len : natural) return signed;
  function sign_extend (datum : signed; proto : signed) return signed;
  function sign_extend (datum : unsigned; len : natural) return unsigned;
  function sign_extend (datum : unsigned; proto : unsigned) return unsigned;
  function zero_pad_lsbs (datum : signed; len : natural) return signed;
  function zero_pad_lsbs (datum : signed; proto : signed) return signed;
  function zero_pad_lsbs (datum : unsigned; len : natural) return unsigned;
  function zero_pad_lsbs (datum : unsigned; proto : unsigned) return unsigned;

  function resize_rnd (datum: signed; length: natural) return signed;
                    
  function resize_rnd1 (datum: signed) return signed;
  function resize_even_rnd (datum: signed; length: natural) return signed;
  function resize_even_rnd (datum: unsigned; length: natural) return unsigned;

  function limited_lshift (din : in signed;  shift : in natural) return signed;
  function resize_ovfl (datum: signed; length: natural) return std_logic;
  function resize_sat_ovfl (overflow: std_logic; datum: signed; length: natural) return signed;

end numeric_csw;

package body numeric_csw is
  function limited_lshift (
    din : in signed;
    shift : in natural)
    return signed is

    variable lsbs     : signed(din'length-2 downto 0) := (others => not din(din'high));
    variable comp_vec : signed(din'high downto 0) := (others => din(din'high));
    variable result   : signed(din'high downto 0);
  begin
    if shift_right(din,din'length-(shift+1)) = shift_right(comp_vec,din'length-(shift+1)) then
      result := shift_left(din, shift);
    else               
                                                                                    
      result := din(din'high) & lsbs;            
    end if;
    return result;
  end limited_lshift;

  function to_integer (
    bit : std_logic
  ) return integer is
  begin
    if '1' = bit then
      return 1;
    else
      return 0;
    end if;
  end to_integer;

  function to_natural (
    bit : std_logic
  ) return natural is
  begin
    if '1' = bit then
      return 1;
    else
      return 0;
    end if;
  end to_natural;

  function to_natural (
    datum : unsigned
  ) return natural is
  begin
    return to_integer(datum);
  end to_natural;

  function to_integer (
    bool  : boolean
  ) return natural is
  begin
    if bool then
      return 1;
    else
      return 0;
    end if;
  end to_integer;

  function to_natural (bool : boolean) return natural is
  begin
    return to_integer(bool);
  end function to_natural;

  function to_signed (
    datum : unsigned
  ) return signed is
  begin
    return signed('0' & datum);
  end to_signed;

  function to_signed (
    datum : real;
    width : natural
  ) return signed is
    variable maxvalue : signed(width-1 downto 0):= (others => '1');
  begin
    assert datum >= -1.0 and (datum <= 1.0) report "Range failure on real to signed conversion. Real needs to be [1.0:-1.0]. Saturating result." severity warning;
    if datum < -1.0 then
      maxvalue := not maxvalue;
      maxvalue(width-1) := '1';
      return maxvalue;
    elsif datum > 1.0 then
      maxvalue(width-1) := '0';
      return maxvalue;
    else
      return to_signed(round(datum * 2.0**(width-1)), width);
    end if;
  end to_signed;

  function to_unsigned (
    datum : real;
    width : natural
  ) return unsigned is
  begin
    return to_unsigned(round(datum * 2.0**width), width);
  end to_unsigned;   

  function to_unsigned (
    bit : std_logic;
    width : positive := 1
  ) return unsigned is
    variable result : unsigned(width-1 downto 0) := (others => '0');
  begin
    result(0) := bit;
    return result;
  end to_unsigned;   

  function to_unsigned (datum : signed; warn : boolean := true) return unsigned is
    variable result : unsigned(datum'length-1 downto 0) := unsigned(datum);
  begin
    assert (not warn) or ('1' /= to_x01(datum(datum'left)))
      report "TO_UNSIGNED removing sign bit of '1'."
      severity error;
    return result(datum'length-2 downto 0);
  end function to_unsigned;

  function to_std_logic (
    bool : boolean
  ) return std_logic is
  begin
    if bool then
      return '1';
    else
      return '0';
    end if;
  end to_std_logic;

  function to_std_logic (num : integer) return std_logic is
  begin
    if 0 = num then
      return '0';
    else
      return '1';
    end if;
  end to_std_logic;

  function to_std_logic_vector (num : integer; width : natural) return std_logic_vector is
    variable tmp : signed(width+0 downto 0);
  begin
    tmp := to_signed(num, width+1);
    return std_logic_vector(tmp(width-1 downto 0));
  end to_std_logic_vector;

  function to_std_logic_vector (bv : boolean_vector) return std_logic_vector is
    variable result : std_logic_vector(bv'range);
  begin
    for ii in bv'range loop
      result(ii) := to_std_logic(bv(ii));
    end loop;
    return result;
  end function to_std_logic_vector;

  function to_boolean_vector (slv : std_logic_vector) return boolean_vector is
    variable slv_01 : std_logic_vector(slv'range) := to_01_bitwise(slv, xmap => '0');
    variable result : boolean_vector(slv'range);
  begin
    for ii in slv'range loop
      result(ii) := ('1' = slv_01(ii));
    end loop;
    return result;
  end function to_boolean_vector;

  function value_max (val : signed) return signed is
    variable result : signed(val'range) := (others => '1');
  begin
    result(result'left) := '0';
    return result;
  end function value_max;
  function value_max (val : unsigned) return unsigned is
    constant result : unsigned(val'range) := (others => '1');
  begin
    return result;
  end function value_max;

  function value_min_wanted (val : signed) return signed is
    variable result : signed(val'range) := (others => '0');
  begin
    result(result'left)  := '1';
    result(result'right) := '1';
    return result;
  end function value_min_wanted;
  function value_min (val : signed) return signed is
    variable result : signed(val'range) := (others => '0');
  begin
    result(result'left) := '1';
    return result;
  end function value_min;
  function value_min (val : unsigned) return unsigned is
    constant result : unsigned(val'range) := (others => '0');
  begin
    return result;
  end function value_min;

  function value_zero (val : signed) return signed is
    constant result : signed(val'range) := (others => '0');
  begin
    return result;
  end function value_zero;
  function value_zero (val : unsigned) return unsigned is
    constant result : unsigned(val'range) := (others => '0');
  begin
    return result;
  end function value_zero;

  function negate (
    num : signed
  ) return signed is
    constant wi : natural := num'length;
    variable max_neg : signed(wi-1 downto 0);
    variable result : signed(wi-1 downto 0);
  begin           
                                     
    max_neg := (others => '0');
    max_neg(max_neg'high) := '1';
    if num = max_neg then
                      
      result := not max_neg;
      return result;
    else
      return -num;
    end if;
  end negate;

  function negate (
    num: unsigned
  ) return signed is
  begin
    return negate(signed('0' & num));
  end negate;

  function remove_max_neg (datum: signed) return signed is
    variable zero : signed(datum'high-1 downto 0) := (others => '0');
    variable zero_ms1 : signed(datum'high-1 downto 1) := (others => '0');
  begin
    if datum = ('1' & zero) then
      return '1' & zero_ms1 & '1';
    else
      return datum;
    end if;
  end remove_max_neg;

  function rmsgn (
    num           : signed;
    warn_overflow : boolean := true;
    saturate      : boolean := true)
  return signed is
    alias num_down  : signed(num'length-1 downto 0) is num;
    variable result : signed(num'length-2 downto 0);
  begin          
                                
    result := num_down(num_down'high-1 downto 0);

    if num_down'length >= 2 then
      if num_down(num_down'high downto num_down'high-1) = "01" then
        if saturate then
          result              := (others => '1');
          result(result'high) := '0';
        end if;
        assert not warn_overflow report "Positive overflow in rmsgn." severity warning;
      elsif num_down(num_down'high downto num_down'high-1) = "10" then
        if saturate then
          result              := (others => '0');
          result(result'high) := '1';
          result(result'low)  := '1';
        end if;
        assert not warn_overflow report "Negative overflow in rmsgn." severity warning;
      end if;
    end if;
    
    return result;
  end function rmsgn;

  function rmsgn (
    num           : unsigned;
    warn_overflow : boolean := true;
    saturate      : boolean := true)
  return unsigned is
    alias num_down  : unsigned(num'length-1 downto 0) is num;
    variable result : unsigned(num'length-2 downto 0);
  begin

    result := num_down(num_down'high-1 downto 0);

    if num_down'length >= 1 then
      if num_down(num_down'high) = '1' then
        if saturate then
          result := (others => '1');
        end if;
        assert not warn_overflow report "Positive overflow in rmsgn." severity warning;
      end if;
    end if;
    
    return result;
  end function rmsgn;

  function wrap_sum (num           : signed;
                     warn_overflow : boolean := false;
                     saturate      : boolean := false) return signed is
  begin
    return rmsgn(num           => num,
                 warn_overflow => warn_overflow,
                 saturate      => saturate);
  end function wrap_sum;
  function wrap_sum (num           : unsigned;
                     warn_overflow : boolean := false;
                     saturate      : boolean := false) return unsigned is
  begin
    return rmsgn(num           => num,
                 warn_overflow => warn_overflow,
                 saturate      => saturate);
  end function wrap_sum;

  function iszeros (
    num: std_logic_vector
  ) return boolean is
    variable allzeros : boolean := true;
  begin
    for ii in num'range loop
      allzeros := allzeros and ('0' = num(ii));
    end loop;       
    return allzeros;
  end iszeros;

  function iszeros (
    num: signed
  ) return boolean is
  begin
    return iszeros(std_logic_vector(num));
  end iszeros;

  function iszeros (
    num: unsigned
  ) return boolean is
  begin
    return iszeros(std_logic_vector(num));
  end iszeros;

  function add (a: signed; b: std_logic) return signed is
    variable b_vec : signed(1 downto 0) := "00";
  begin
    b_vec(0) := b;
    return a + b_vec;
  end function add;

  function add (a: unsigned; b: std_logic) return unsigned is
    variable b_vec : unsigned(0 downto 0);
  begin
    b_vec(0) := b;
    return a + b_vec;
  end function add;

  function add (a: std_logic; b: signed) return signed is
  begin
    return add(b,a);
  end function add;

  function add (a: std_logic; b: unsigned) return unsigned is
  begin
    return add(b,a);
  end function add;

  function add (a: integer; b: std_logic) return integer is
  begin
    if '1' = to_01(b) then
      return a + 1;
    else
      return a;
    end if;
  end function add;
  function "+" (a: integer; b: std_logic) return integer is
  begin
    return add(a, b);
  end function "+";

  function add (a: std_logic; b: integer) return integer is
  begin
    return add(b, a);
  end function add;
  function "+" (a: std_logic; b: integer) return integer is
  begin
    return add(b, a);
  end function "+";

  function diff (a: signed; b: std_logic) return signed is
    variable b_vec : signed(1 downto 0) := "00";
  begin
    b_vec(0) := b;
    return a - b_vec;
  end function diff;

  function diff (a: unsigned; b: std_logic) return unsigned is
    variable b_vec : unsigned(0 downto 0);
  begin
    b_vec(0) := b;
    return a - b_vec;
  end function diff;

  function diff (a: std_logic; b: signed) return signed is
    variable a_vec : signed(1 downto 0) := "00";
  begin
    a_vec(0) := a;
    return a_vec - b;
  end function diff;

  function diff (a: std_logic; b: unsigned) return unsigned is
    variable a_vec : unsigned(0 downto 0);
  begin
    a_vec(0) := a;
    return a_vec - b;
  end function diff;

  function diff (a : integer; b : std_logic) return integer is
  begin
    if '1' = to_01(b) then
      return a - 1;
    else
      return a - 0;
    end if;
  end function diff;
  function "-" (a : integer; b : std_logic) return integer is
  begin
    return diff(a, b);
  end function "-";

  function diff (a : std_logic; b : integer) return integer is
  begin
    if '1' = to_01(a) then
      return 1 - b;
    else
      return 0 - b;
    end if;
  end function diff;
  function "-" (a : std_logic; b : integer) return integer is
  begin
    return diff(a, b);
  end function "-";

  function add (a: unsigned; b: signed) return signed is
  begin
    return to_signed(a) + b;
  end function add;
  function "+" (a: unsigned; b: signed) return signed is
  begin
    return add(a, b);
  end function "+";

  function add (a: signed; b: unsigned) return signed is
  begin
    return add(b, a);
  end function add;
  function "+" (a: signed; b: unsigned) return signed is
  begin
    return add(a, b);
  end function "+";

  function diff (a: unsigned; b: signed) return signed is
  begin
    return to_signed(a) - b;
  end function diff;
  function "-" (a: unsigned; b: signed) return signed is
  begin
    return diff(a, b);
  end function "-";
  function diff (a: signed; b: unsigned) return signed is
  begin
    return a - to_signed(b);
  end function diff;
  function "-" (a: signed; b: unsigned) return signed is
  begin
    return diff(a, b);
  end function "-";

  function prod (a : signed; b : unsigned) return signed is
    variable b_sgn   : signed(b'length+0 downto 0) := '0' & signed(b);
    variable product : signed(a'length+b'length downto 0);
  begin
    product := a * b_sgn;
    return product(product'high-1 downto 0);
  end prod;
  function "*" (a : signed; b : unsigned) return signed is
  begin
    return prod(a,b);
  end "*";

  function prod (a : unsigned; b : signed) return signed is
  begin
    return prod(b,a);
  end prod;
  function "*" (a : unsigned; b : signed) return signed is
  begin
    return prod(a,b);
  end "*";

  function prod (a : signed; b : std_logic) return signed is
    variable zeros : signed(a'range) := (others => '0');
  begin 
    if '1' = b then
      return a;
    else
      return zeros;
    end if;
  end prod;
  function "*" (a : signed; b : std_logic) return signed is
  begin 
    return prod(a,b);
  end "*";

  function prod (a : std_logic; b : signed) return signed is
  begin
    return prod(b,a);
  end function prod;
  function "*" (a : std_logic; b : signed) return signed is
  begin
    return prod(a,b);
  end function "*";

  function prod (a : unsigned; b : std_logic) return unsigned is
    variable result : unsigned(a'range) := (others => '0');
  begin
    if '1' = b then
      return a;
    else
      return result;
    end if;
  end prod;
  function "*" (a : unsigned; b : std_logic) return unsigned is
  begin
    return prod(a,b);
  end "*";

  function prod (a : std_logic; b : unsigned) return unsigned is
  begin
    return prod(b,a);
  end function prod;
  function "*" (a : std_logic; b : unsigned) return unsigned is
  begin
    return prod(a,b);
  end function "*";

  function prod (a : std_logic_vector; b : std_logic) return std_logic_vector is
    variable result : std_logic_vector(a'range) := (others => '0');
  begin
    if '1' = b then
      return a;
    else
      return result;
    end if;
  end prod;  
  function "*" (a : std_logic_vector; b : std_logic) return std_logic_vector is
  begin
    return prod(a,b);
  end "*";
  function prod (b : std_logic; a : std_logic_vector) return std_logic_vector is
  begin
    return prod(a,b);
  end function prod;
  function "*" (b : std_logic; a : std_logic_vector) return std_logic_vector is
  begin
    return prod(b,a);
  end function "*";

  function ">" (l : unsigned; r : signed) return boolean is
  begin
    return to_integer(l) > to_integer(r);
  end function ">";
  
  function ">" (l : signed; r : unsigned) return boolean is
  begin
    return to_integer(l) > to_integer(r);
  end function ">";

  function "<" (l : unsigned; r : signed) return boolean is
  begin
    return to_integer(l) < to_integer(r);
  end function "<";

  function "<" (l : signed; r : unsigned) return boolean is
  begin
    return to_integer(l) < to_integer(r);
  end function "<";

  function vector_or (vec : std_logic_vector) return std_logic is
    variable result : std_logic;
  begin
    result := '0';
    for ii in vec'range loop
      result := result or vec(ii);
    end loop;
    return result;
  end vector_or;

  function vector_or (vec : unsigned) return std_logic is
  begin
    return vector_or(std_logic_vector(vec));
  end vector_or;

  function vector_or (vec : signed) return std_logic is
  begin
    return vector_or(std_logic_vector(vec));
  end vector_or;

  function vector_and (vec : std_logic_vector) return std_logic is
    variable result : std_logic;
  begin
    result := '1';
    for ii in vec'range loop
      result := result and vec(ii);
    end loop;
    return result;
  end vector_and;

  function vector_and (vec : unsigned) return std_logic is
  begin
    return vector_and(std_logic_vector(vec));
  end vector_and;

  function vector_and (vec : signed) return std_logic is
  begin
    return vector_and(std_logic_vector(vec));
  end vector_and;

  function vector_xor (vec : std_logic_vector) return std_logic is
    variable result : std_logic;
  begin
    result := '0';
    for ii in vec'range loop
      result := result xor vec(ii);
    end loop;
    return result;
  end vector_xor;

  function vector_xor (vec : unsigned) return std_logic is
  begin
    return vector_xor(std_logic_vector(vec));
  end vector_xor;

  function vector_xor (vec : signed) return std_logic is
  begin
    return vector_xor(std_logic_vector(vec));
  end vector_xor;

  function shift_left_logical (vec : std_logic_vector; nbits : natural) return std_logic_vector is
    variable vec_dn    : std_logic_vector(vec'length-1 downto 0) := vec;
    variable result_dn : std_logic_vector(vec'length-1 downto 0) := (others => '0');
    variable result    : std_logic_vector(vec'range);
    variable vec_ii    : integer;
  begin
    for ii in result_dn'range loop
      vec_ii     := ii - nbits;
      if 0 <= vec_ii and vec_ii < vec'length then
       result_dn(ii) := vec_dn(vec_ii);
      end if;
    end loop;
    result := result_dn;
    return result;
  end function shift_left_logical;

  function  shift_right_logical (vec : std_logic_vector; nbits : natural) return std_logic_vector is
    variable vec_dn    : std_logic_vector(vec'length-1 downto 0) := vec;
    variable result_dn : std_logic_vector(vec'length-1 downto 0) := (others => '0');
    variable result    : std_logic_vector(vec'range);
    variable vec_ii    : integer;
  begin
    for ii in result_dn'range loop
      vec_ii     := ii + nbits;
      if 0 <= vec_ii and vec_ii < vec'length then
       result_dn(ii) := vec_dn(vec_ii);
      end if;
    end loop;
    result := result_dn;
    return result;
  end function  shift_right_logical;

  function circle_rotate_left (vec : std_logic_vector; nbits : integer) return std_logic_vector is
    variable vec_dn    : std_logic_vector(vec'length-1 downto 0) := vec;
    variable result_dn : std_logic_vector(vec'length-1 downto 0) := (others => '0');
    variable result    : std_logic_vector(vec'range);
    variable nbits_mod : integer;
    variable vec_ii    : integer;
  begin
                                                           
    if vec'length < 1 then
      return vec;
    end if;
                                                            
    nbits_mod := nbits mod vec'length;
    if 0 = nbits_mod then
      return vec;
    end if;
                    
    for ii in result_dn'range loop
      vec_ii := ii - nbits_mod;
      if 0   <= vec_ii and vec_ii < vec'length then
        result_dn(ii) := vec_dn(vec_ii);
      end if;
      vec_ii := ii - nbits_mod + vec'length;
      if 0   <= vec_ii and vec_ii < vec'length then
        result_dn(ii) := vec_dn(vec_ii);
      end if;
    end loop;
    result := result_dn;
    return result;
  end function circle_rotate_left;

  function circle_rotate_right (vec : std_logic_vector; nbits : integer) return std_logic_vector is
    variable vec_dn    : std_logic_vector(vec'length-1 downto 0) := vec;
    variable result_dn : std_logic_vector(vec'length-1 downto 0) := (others => '0');
    variable result    : std_logic_vector(vec'range);
    variable nbits_mod : integer;
    variable vec_ii    : integer;
  begin
                                                           
    if vec'length < 1 then
      return vec;
    end if;
                                                            
    nbits_mod := nbits mod vec'length;
    if 0 = nbits_mod then
      return vec;
    end if;
                    
    for ii in result_dn'range loop
      vec_ii := ii + nbits_mod;
      if 0   <= vec_ii and vec_ii < vec'length then
        result_dn(ii) := vec_dn(vec_ii);
      end if;
      vec_ii := ii + nbits_mod - vec'length;
      if 0   <= vec_ii and vec_ii < vec'length then
        result_dn(ii) := vec_dn(vec_ii);
      end if;
    end loop;
    result := result_dn;
    return result;
  end function circle_rotate_right;

  function logical_and (
    l : std_logic;
    r : std_logic_vector)
  return std_logic_vector is 
    variable result : std_logic_vector(r'range);
  begin 
    for ii in result'range loop
      result(ii) := l and r(ii);
    end loop;
    return result;
  end function logical_and;

  function logical_and (
    l : std_logic_vector;
    r : std_logic)
  return std_logic_vector is 
  begin
    return logical_and(r,l);
  end function logical_and;

  function exclusive_or (
    l : std_logic;
    r : std_logic_vector)
  return std_logic_vector is 
    variable result : std_logic_vector(r'range);
  begin 
    for ii in result'range loop
      result(ii) := l xor r(ii);
    end loop;
    return result;
  end function exclusive_or;

  function exclusive_or (
    l : std_logic_vector;
    r : std_logic)
  return std_logic_vector is 
  begin
    return r xor l;
  end function exclusive_or;

  function logical_and (l : std_ulogic; r : boolean) return std_logic is
  begin
    return l and to_std_logic(r);
  end function logical_and;
  
  function logical_and (l : boolean; r : std_ulogic) return std_logic is
  begin
    return to_std_logic(l) and r;
  end function logical_and;
  
  function logical_or (l  : std_ulogic; r : boolean) return std_logic is
  begin
    return l or to_std_logic(r);
  end function logical_or;
  
  function logical_or (l  : boolean; r : std_ulogic) return std_logic is
  begin
    return to_std_logic(l) or r;
  end function logical_or;
    
  function "and" (l : std_ulogic; r : boolean) return std_logic is
  begin
    return logical_and(l,r);
  end function "and";
  
  function "and" (l : boolean; r : std_ulogic) return std_logic is
  begin
    return logical_and(l,r);
  end function "and";
  
  function "or" (l  : std_ulogic; r : boolean) return std_logic is
  begin
    return logical_or(l,r);
  end function "or";
  
  function "or" (l  : boolean; r : std_ulogic) return std_logic is
  begin
    return logical_or(l,r);
  end function "or";

  function to_01_bitwise(vec : std_logic_vector; xmap : std_logic := '0') return std_logic_vector is
    variable result : std_logic_vector(0 to vec'length-1);
    alias vec_up    : std_logic_vector(0 to vec'length-1) is vec;
  begin
    for i in result'range loop
      case vec_up(i) is
        when '0' | 'L' => result(i) := '0';
        when '1' | 'H' => result(i) := '1';
        when others    => result(i) := xmap;
      end case;
    end loop;
    return result;
  end function to_01_bitwise;

  function to_01_bitwise(vec : signed; xmap : std_logic := '0') return signed is
  begin
    return signed(to_01_bitwise(std_logic_vector(vec)));
  end function to_01_bitwise;

  function to_01_bitwise(vec : unsigned; xmap : std_logic := '0') return unsigned is
  begin
    return unsigned(to_01_bitwise(std_logic_vector(vec)));
  end function to_01_bitwise;

  function to_01_bw(vec : std_logic_vector; xmap : std_logic := '0') return std_logic_vector is
  begin
    return to_01_bitwise(vec, xmap);
  end function to_01_bw;
                                          
  function to_01_bw(vec : unsigned; xmap : std_logic := '0') return unsigned is
  begin
    return to_01_bitwise(vec, xmap);
  end function to_01_bw;
                                          
  function to_01_bw(vec : signed; xmap : std_logic := '0') return signed is
  begin
    return to_01_bitwise(vec, xmap);
  end function to_01_bw;

  function count_ones(vec : std_logic_vector) return natural is
    variable count : natural := 0;
  begin
    for ii in vec'range loop
      if '1' = to_x01(vec(ii)) then
        count := count + 1;
      end if;
    end loop;
    return count;
  end function count_ones;

  function byte_reverse (sig : std_logic_vector) return std_logic_vector is
    constant wi_sig_bytes : natural := sig'length / 8;
    
    variable sig_up : std_logic_vector(0 to sig'length-1) := sig;
    variable result : std_logic_vector(0 to sig'length-1);
    variable lo1, lo2 : integer;
  begin
    lo1 := 0;
    lo2 := sig'length - 8;
    for ii in 0 to wi_sig_bytes-1 loop
      result(lo1 to lo1+7) := sig_up(lo2 to lo2+7);
      lo1 := lo1 + 8;
      lo2 := lo2 - 8;
    end loop;
    return result;
  end function byte_reverse;

  function bit_reverse (sig : std_logic_vector) return std_logic_vector is
    variable result : std_logic_vector(sig'reverse_range);
  begin
    for ii in sig'range loop
      result(ii) := sig(ii);
    end loop;
    return result;
  end function bit_reverse;

  function find_rightmost(vec : std_logic_vector; val : std_ulogic) return integer is
  begin
    for pos in vec'reverse_range loop
      if to_x01z(vec(pos)) = val then
        return pos;
      end if;
    end loop;
    return -1;
  end function find_rightmost;

  function find_leftmost(vec : std_logic_vector; val : std_ulogic) return integer is
  begin
    for pos in vec'range loop
      if to_x01z(vec(pos)) = val then
        return pos;
      end if;
    end loop;
    return -1;
  end function find_leftmost;

  function hex_value (ch : character) return integer is
  begin
    case ch is
      when '0' to '9' =>
        return character'pos(ch) - character'pos('0');
      when 'a' to 'f' =>
        return 10 + character'pos(ch) - character'pos('a');
      when 'A' to 'F' =>
        return 10 + character'pos(ch) - character'pos('A');

      when others =>
        assert false report "Invalid character ["&character'image(ch)&"] to hex_value." severity failure;
        return -1;
                     
    end case;
  end function hex_value;

  procedure check_discard_literal_bits (val : signed; len : natural)
  is
    variable extra_bits : std_logic_vector(val'length-1 downto len);
    variable val_slv    : std_logic_vector(val'length-1 downto 0)
      := std_logic_vector(val);
  begin
    if 0 < extra_bits'length then
      extra_bits := val_slv(extra_bits'range);

      if '0' /= vector_or(extra_bits) then
        if '0' = val_slv(len-1) then
          assert false report "Discarded significant bits." severity failure;
        elsif '1' /= vector_and(extra_bits) then
          assert false report "Discarded significant bits." severity failure;
        end if;
      end if;
                   
    end if;
  end procedure check_discard_literal_bits;

  procedure check_discard_literal_bits (val : unsigned; len : natural) is
  begin
                  
    assert (len-1 = val'left) or ('0' = vector_or(val(val'left downto len)))
      report "Lost non-zero bits in conversion."
      severity failure;
                 
  end procedure check_discard_literal_bits;

  function from_hstring(str : string; len : natural) return std_logic_vector is
    variable result_uns : unsigned(len-1 downto 0);
  begin
    result_uns := from_hstring(str, len);
    return std_logic_vector(result_uns);
  end function from_hstring;

  function from_hstring(str : string; len : natural) return unsigned is
    constant num_str_digits : natural := str'length;
    constant num_vec_digits : natural := maximum(ceildiv(len, 4), num_str_digits);
    variable result         : unsigned(4*num_vec_digits-1 downto 0) := (others => '0');
  begin
                
    if 0 = len then
      return result;
    end if;

    for ii in str'range loop
      result             := result rol 4;
      result(3 downto 0) := to_unsigned(hex_value(str(ii)), 4);

    end loop;

    check_discard_literal_bits(result, len);
    
    return result(len-1 downto 0);
  end function from_hstring;

  function from_hstring(str : string; len : natural) return signed is
    constant num_str_digits : natural := str'length;
    constant num_vec_digits : natural := maximum(ceildiv(len, 4), num_str_digits);
    variable result_uns     : unsigned(4*num_vec_digits-1 downto 0);
    variable result_wide    : signed(4*num_vec_digits-1 downto 0);
  begin
    result_uns  := from_hstring(str, result_uns'length);
    result_wide := signed(result_uns);
    check_discard_literal_bits(result_wide, len);
    return result_wide(len-1 downto 0);
  end function from_hstring;

  function from_ostring (str : string; len : natural) return unsigned is
    constant num_str_digits : natural := str'length;
    constant num_vec_digits : natural := maximum(ceildiv(len, 3), num_str_digits);
    variable result         : unsigned(4*num_vec_digits-1 downto 0) := (others => '0');
  begin
                
    if 0 = len then
      return result;
    end if;

    for ii in str'range loop
      result             := result rol 3;
      result(2 downto 0) := to_unsigned(integer'value(str(ii to ii)), 3);
    end loop;

    check_discard_literal_bits(result, len);
    
    return result(len-1 downto 0);
  end function from_ostring;

  function from_ostring (str : string; len : natural) return std_logic_vector is
    variable result_uns : unsigned(len-1 downto 0);
  begin
    result_uns := from_ostring(str, len);
    return std_logic_vector(result_uns);
  end function from_ostring;

  function from_ostring (str : string; len : natural) return signed is
    constant num_str_digits : natural := str'length;
    constant num_vec_digits : natural := maximum(ceildiv(len, 3), num_str_digits);
    variable result_uns     : unsigned(3*num_vec_digits-1 downto 0);
    variable result_wide    : signed(3*num_vec_digits-1 downto 0);
  begin
    result_uns  := from_ostring(str, result_uns'length);
    result_wide := signed(result_uns);                               
    check_discard_literal_bits(result_wide, len);
    return result_wide(len-1 downto 0);
  end function from_ostring;

  function from_bstring (str : string) return unsigned is
    variable tmp    : unsigned(str'range);
    variable result : unsigned(str'length-1 downto 0);
  begin
    for ii in str'range loop
      tmp(ii) := std_logic'value(str(ii to ii));
    end loop;
    result := tmp;
    return result;
  end function from_bstring;

  function from_bstring (str : string) return std_logic_vector is
    variable value_uns : unsigned(str'length-1 downto 0);
  begin
    value_uns := from_bstring(str);
    return std_logic_vector(value_uns);
  end function from_bstring;

  function from_bstring (str : string) return signed is
    variable value_uns : unsigned(str'length-1 downto 0);
  begin
    value_uns := from_bstring(str);
    return signed(value_uns);
  end function from_bstring;

  function from_dstring (str : string; len : natural) return unsigned is
                                                     
    constant num_str_digits : natural                               := str'length;
    constant num_vec_digits : natural                               := maximum(ceildiv(len, 4), num_str_digits);
    variable result         : unsigned(4*num_vec_digits-1 downto 0) := (others => '0');
    variable result_mac     : unsigned(2*result'length-1 downto 0);
  begin
                
    if 0 = len then
      return result;
    end if;

    result := (others => '0');
    for ii in str'range loop
      result_mac := result * 10 + integer'value(str(ii to ii));
      result     := result_mac(result'range);
    end loop;

    check_discard_literal_bits(result, len);
    
    return result(len-1 downto 0);
  end function from_dstring;

  function from_dstring (str : string; len : natural) return std_logic_vector is
    variable result_uns : unsigned(len-1 downto 0);
  begin
    result_uns := from_dstring(str, len);
    return std_logic_vector(result_uns);
  end function from_dstring;

  function from_dstring (str : string; len : natural) return signed is
    constant num_str_digits : natural := str'length;
    constant num_vec_digits : natural := maximum(ceildiv(len, 4), num_str_digits);
    variable result_uns     : unsigned(4*num_vec_digits-1 downto 0);
    variable result_wide    : signed(4*num_vec_digits-1 downto 0);
  begin
    result_uns  := from_dstring(str, result_uns'length);
    result_wide := signed(result_uns);
    check_discard_literal_bits(result_wide, len);
    return result_wide(len-1 downto 0);
  end function from_dstring;

  procedure dec_write(variable ln : inout line; val : std_logic) is
  begin
    write(ln, val);
  end procedure dec_write;
  
  procedure dec_write(variable ln : inout line; val : signed) is
  begin
    write(ln, to_integer(val));
  end procedure dec_write;
  
  procedure dec_write(variable ln : inout line; val : unsigned) is
  begin
    write(ln, to_integer(val));
  end procedure dec_write;
  
  procedure dec_write(variable ln : inout line; val : std_logic_vector) is
  begin
    dec_write(ln, unsigned(val));
  end procedure dec_write;

  function master_resize (
    datum  : std_logic_vector;                                     
    addb   : resize_num_add_bits;                                             
    sflag  : boolean;                                                
    method : resize_method                                           
  ) return std_logic_vector is

    variable wi_resmsb  : integer := datum'length-1 + maximum(0, addb(1));
    variable wi_reslsb  : integer := datum'length-1 + addb(2) + maximum(0, addb(1));
    variable wi_result  : integer := datum'length-1 + addb(2) + addb(1);
    variable wi_half    : natural := maximum(1, -addb(2));
    variable wi_widesgn : natural := maximum(1, -addb(1)+1);
    variable datum_dn : std_logic_vector(datum'length-1 downto 0) := datum;
    variable half       : std_logic_vector(wi_half-1 downto 0);
    variable widesgn    : std_logic_vector(wi_widesgn-1 downto 0);
    variable resmsb     : std_logic_vector(wi_resmsb downto 0);
    variable reslsb     : std_logic_vector(wi_reslsb downto 0);
    variable result     : std_logic_vector(wi_result downto 0);
  begin                  

    if addb(1) > 0 then    
                                                        
      if sflag then
                                                     
        resmsb := (others => datum_dn(datum_dn'high));
        resmsb(datum_dn'length-1 downto 0) := datum_dn;
      else
                                                                
        resmsb := (others => '0');
        resmsb(datum_dn'length-1 downto 0) := datum_dn;
      end if;
    else
      resmsb := datum_dn;
    end if;

    if 0 = addb(2) then
      reslsb := resmsb;
    elsif addb(2) < 0 then
      if resize_nosat = method or resize_trunc = method then
                    
        reslsb := resmsb(resmsb'high downto -addb(2)+resmsb'low);
      else
                                  
        half := (others => '0');
        half(half'high) := '1';

        if half = resmsb(-addb(2)+resmsb'low-1 downto resmsb'low) then
          if (method = resize_roundne and '1' = resmsb(-addb(2)+resmsb'low)) or
             (method = resize_roundno and '0' = resmsb(-addb(2)+resmsb'low)) or
             (method = resize_roundtz and (    sflag and '1' = resmsb(resmsb'high))) or
             (method = resize_roundaz and (not sflag or  '0' = resmsb(resmsb'high))) then
                        
              reslsb := std_logic_vector(unsigned(resmsb(resmsb'high downto -addb(2)+resmsb'low)) + 1);
          else
                                           
              reslsb := resmsb(resmsb'high downto -addb(2)+resmsb'low);
          end if;
        elsif '1' = resmsb(-addb(2)+resmsb'low-1) then
                                          
          reslsb := std_logic_vector(unsigned(resmsb(resmsb'high downto -addb(2)+resmsb'low)) + 1);
        else
                                  
          reslsb := resmsb(resmsb'high downto -addb(2)+resmsb'low);
        end if;
      end if;
    else
                               
      reslsb := (others => '0');
      reslsb(reslsb'high downto addb(2)) := resmsb;
    end if;

    result := reslsb(reslsb'high+minimum(0,addb(1)) downto 0);
    if resize_nosat /= method then
      if sflag then
                            
        widesgn := resize(datum_dn(datum_dn'high), widesgn'length);
        if ('0' = datum_dn(datum_dn'high) and widesgn /= reslsb(reslsb'high downto reslsb'high-widesgn'high)) or                       
           ('1' = datum_dn(datum_dn'high) and widesgn /= datum_dn(datum_dn'high downto datum_dn'high-widesgn'high)) or                       
           ('1' = reslsb(reslsb'high) and '0' = vector_or(reslsb(reslsb'high-widesgn'high-1 downto 0))) then                                     
          if '1' = datum_dn(datum_dn'high) then
                                                                             
            result := (others => '0');
            result(result'high) := '1';
            result(0) := '1';
          else
                                
            result(result'high) := '0';
            result(result'high-1 downto 0) := (others => '1');
          end if;                                             
        end if;                           
      else
                              
        if (0 /= addb(1) and '0' /= vector_or(reslsb(reslsb'high downto reslsb'high-widesgn'high+1))) or                       
           ('1' = datum_dn(datum_dn'high) and '0' = reslsb(reslsb'high)) then                       
                              
          result := (others => '1');
        end if;                           
      end if;                                
    end if;                         

    return result;
  end master_resize;

  function calc_addbits (
    oldlen : natural;
    newlen : natural;
    align  : resize_alignment
  ) return resize_num_add_bits is
    variable result : resize_num_add_bits;
  begin                 
    if align_msb = align then
      result := (0, newlen-oldlen);
    else
      result := (newlen-oldlen, 0);
    end if;
    return result;
  end calc_addbits;

  function resize (datum : std_logic; length : integer) return std_logic_vector is
    variable result : std_logic_vector(length-1 downto 0);
  begin           
    result := (others => datum);
    return result;
  end function resize;

  function resize (
    datum  : signed;
    length : integer;
    align  : resize_alignment;
    method : resize_method := resize_nosat
  ) return signed is
    variable addbits : resize_num_add_bits;
  begin           
                                      
    if datum'length = length then
      return datum;
    else
      addbits := calc_addbits(datum'length, length, align);
      return signed(master_resize(std_logic_vector(datum), addbits, true, method));
    end if; 
  end function resize;

  function resize (
    datum  : unsigned;
    length : integer;
    align  : resize_alignment;
    method : resize_method := resize_nosat
  ) return unsigned is
    variable addbits : resize_num_add_bits;
  begin           
                                      
    if datum'length = length then
      return datum;
    else
      addbits := calc_addbits(datum'length, length, align);
      return unsigned(master_resize(std_logic_vector(datum), addbits, false, method));
    end if;
  end function resize;

  function resize (
    datum  : std_logic_vector;
    length : integer;
    align  : resize_alignment
  ) return std_logic_vector is
    variable addbits : resize_num_add_bits;
  begin
                                      
    if datum'length = length then
      return datum;
    else
      addbits := calc_addbits(datum'length, length, align);
      return master_resize(datum, addbits, false, resize_nosat);
    end if;
  end function resize;

  function resize (
    datum  : signed;
    addb   : resize_num_add_bits;
    method : resize_method := resize_nosat
  ) return signed is
  begin
    return signed(master_resize(std_logic_vector(datum), addb, true, method));
  end function resize;

  function resize (
    datum  : unsigned;
    addb   : resize_num_add_bits;
    method : resize_method := resize_nosat
  ) return unsigned is
  begin
    return unsigned(master_resize(std_logic_vector(datum), addb, false, method));
  end function resize;

  function resize (
    datum : std_logic_vector;
    addb  : resize_num_add_bits
  ) return std_logic_vector is
  begin
    return master_resize(datum, addb, false, resize_nosat);
  end function resize;

  function resize_pre_rnd (datum : signed; length : natural) return signed is
    variable datum_dn   : signed(datum'length-1 downto 0)         := datum;
    variable ones       : signed(length-1 downto 0)               := (others => '1');
    variable max_pos_wf : signed(length downto 0)                 := '0' & ones;
    variable ones_2sh   : signed(length-2 downto 1)               := (others => '1');
    variable max_ps_ms1 : signed(length-1 downto 0)               := '0' & ones_2sh & '0';
    variable rem_zero   : signed(datum_dn'high-length-1 downto 0) := (others => '0');
    variable rem_1zero  : signed(datum_dn'high-length downto 0)   := '1' & rem_zero;
    variable results    : signed(datum_dn'high downto datum_dn'high-length);
  begin
    if (datum_dn(datum_dn'high downto datum_dn'high-length) = max_pos_wf) then
      results := max_pos_wf(length downto 1) & '0';
    elsif (datum_dn(datum_dn'high) = '1') and (datum_dn(datum_dn'high-length downto 0) = rem_1zero) then
      results := datum_dn(datum_dn'high downto datum_dn'high-length+1) & '0';
    else
      results := datum_dn(datum_dn'high downto datum_dn'high-length);
    end if;
    return results;
  end function resize_pre_rnd;

  function resize_fin_rnd (datum : signed) return signed is
    variable datum_dn : signed(datum'length-1 downto 0) := datum;
    variable results  : signed(datum_dn'high-1 downto 0);
  begin
    results := datum_dn(datum_dn'high downto 1) + signed'('0' & datum_dn(0));
    return results;
  end function resize_fin_rnd;

  function sign_extend (datum : signed; len : natural) return signed is
    variable proto : signed(len-1 downto 0);
  begin
    assert len >= datum'length
      report "Input too large for requested length."
      severity error;
    return sign_extend(datum, proto);
  end function sign_extend;
  function sign_extend (datum : signed; proto : signed) return signed is
    variable result_dn : signed(proto'length-1 downto 0);
    variable result    : signed(proto'range);
  begin
    assert proto'length >= datum'length
      report "Input too large for prototype."
      severity error;
    result_dn                          := (others => datum(datum'left));
    result_dn(datum'length-1 downto 0) := datum;
    result                             := result_dn;                     
    return result;
  end function sign_extend;

  function sign_extend (datum : unsigned; len : natural) return unsigned is
    variable proto : unsigned(len-1 downto 0);
  begin
    assert len >= datum'length
      report "Input too large for requested length."
      severity error;
    return sign_extend(datum, proto);
  end function sign_extend;
  function sign_extend (datum : unsigned; proto : unsigned) return unsigned is
    variable result_dn : unsigned(proto'length-1 downto 0);
    variable result    : unsigned(proto'range);
  begin
    assert proto'length >= datum'length
      report "Input too large for prototype."
      severity error;
    result_dn                          := (others => '0');
    result_dn(datum'length-1 downto 0) := datum;
    result                             := result_dn;                     
    return result;
  end function sign_extend;

  function zero_pad_lsbs (datum : signed; len : natural) return signed is
    constant proto : signed(len-1 downto 0) := (others => '0');
  begin
    assert len >= datum'length
      report "Input too large for requested length."
      severity error;
    return zero_pad_lsbs(datum, proto);
  end function zero_pad_lsbs;
  function zero_pad_lsbs (datum : signed; proto : signed) return signed is
    variable result_up : signed(0 to proto'length-1);
    variable result    : signed(proto'range);
  begin
    assert proto'length >= datum'length
      report "Input too large for prototype."
      severity error;
    result_up                      := (others => '0');
    result_up(0 to datum'length-1) := datum;
    result                         := result_up;
    return result;
  end function zero_pad_lsbs;

  function zero_pad_lsbs (datum : unsigned; len : natural) return unsigned is
    constant proto : unsigned(len-1 downto 0) := (others => '0');
  begin
    assert len >= datum'length
      report "Input too large for requested length."
      severity error;
    return zero_pad_lsbs(datum, proto);
  end function zero_pad_lsbs;
  function zero_pad_lsbs (datum : unsigned; proto : unsigned) return unsigned is
    variable result_up : unsigned(0 to proto'length-1);
    variable result    : unsigned(proto'range);
  begin
    assert proto'length >= datum'length
      report "Input too large for prototype."
      severity error;
    result_up                      := (others => '0');
    result_up(0 to datum'length-1) := datum;
    result                         := result_up;
    return result;
  end function zero_pad_lsbs;

  function resize_rnd (datum : signed; length : natural) return signed is
    variable datum_dn    : signed(datum'length-1 downto 0)         := datum;
    variable ones        : signed(length-2 downto 0)               := (others => '1');
    variable max_pos     : signed(length-1 downto 0)               := '0' & ones;
    variable zeros       : signed(length-2 downto 0)               := (others => '0');
    variable max_neg     : signed(length-1 downto 0)               := '1' & zeros;
    variable zeros_sh1   : signed(length-2 downto 1)               := (others => '0');
    variable max_neg_ps1 : signed(length-1 downto 0)               := '1' & zeros_sh1 & '1';
    variable rem_zero    : signed(datum_dn'high-length-1 downto 0) := (others => '0');
    variable rem_1zero   : signed(datum_dn'high-length downto 0)   := '1' & rem_zero;
    variable results     : signed(datum_dn'high downto datum_dn'high-length+1);
  begin
    if (datum_dn(datum_dn'high downto datum_dn'high-length+1) = max_neg) then
      results := max_neg_ps1;
    elsif (datum_dn(datum_dn'high downto datum_dn'high-length+1) = max_pos) or
      ((datum_dn(datum_dn'high) = '1') and (datum_dn(datum_dn'high-length downto 0) = rem_1zero)) then
      results := datum_dn(datum_dn'high downto datum_dn'high-length+1);
    else
      results := datum_dn(datum_dn'high downto datum_dn'high-length+1) + signed'('0' & datum_dn(datum_dn'high-length));
    end if;
    return results;
  end function resize_rnd;

  function resize_rnd1 (datum : signed) return signed is
    variable datum_dn    : signed(datum'length-1 downto 0) := datum;
    variable ones        : signed(datum_dn'high-2 downto 0)   := (others => '1');
    variable max_pos     : signed(datum_dn'high-1 downto 0)   := '0' & ones;
    variable zeros       : signed(datum_dn'high-2 downto 0)   := (others => '0');
    variable max_neg     : signed(datum_dn'high-1 downto 0)   := '1' & zeros;
    variable zeros_sh1   : signed(datum_dn'high-2 downto 1)   := (others => '0');
    variable max_neg_ps1 : signed(datum_dn'high-1 downto 0)   := '1' & zeros_sh1 & '1';
    variable results     : signed(datum_dn'high-1 downto 0);
  begin
    if (datum_dn(datum_dn'high downto 1) = max_neg) then
      results := max_neg_ps1;
    elsif (datum_dn(datum_dn'high downto 1) = max_pos) or
      ((datum_dn(datum_dn'high) = '1') and (datum_dn(0) = '1')) then
      results := datum_dn(datum_dn'high downto 1);
    else
      results := datum_dn(datum_dn'high downto 1) + signed'('0' & datum_dn(0));
    end if;
    return results;
  end function resize_rnd1;

  function resize_even_rnd (datum : signed; length : natural) return signed is
    variable datum_dn   : signed(datum'length-1 downto 0)         := datum;
    variable zero        : signed(datum_dn'high-length-1 downto 0)   := (others => '0');
    variable zero_1_zero : signed(datum_dn'high-length + 1 downto 0) := '0' & '1' & zero;
                                                                                           
    variable ones        : signed(length-2 downto 0)              := (others => '1');
    variable max_pos     : signed(length-1 downto 0)              := '0' & ones;
    variable results     : signed(datum_dn'high downto datum_dn'high-length+1);
  begin

    if datum_dn'length = length then
      results := datum_dn;
                 
    elsif (datum_dn(datum_dn'high-length+1 downto 0) = zero_1_zero) or (datum_dn(datum_dn'high downto datum_dn'high-length+1) = max_pos) then
      results := datum_dn(datum_dn'high downto datum_dn'high-length+1);
    else
      results := datum_dn(datum_dn'high downto datum_dn'high-length+1) + signed'('0' & datum_dn(datum_dn'high-length));
    end if;
    return results;
  end function resize_even_rnd;

  function resize_even_rnd (datum : unsigned; length : natural) return unsigned is
    variable datum_dn    : unsigned(datum'length-1 downto 0)           := datum;
    variable zero        : unsigned(datum_dn'high-length-1 downto 0)   := (others => '0');
    variable zero_1_zero : unsigned(datum_dn'high-length + 1 downto 0) := '0' & '1' & zero;
    variable max_pos     : unsigned(length-1 downto 0)                 := (others => '1');
    variable results     : unsigned(datum_dn'high downto datum_dn'high-length+1);
  begin
    if (datum_dn(datum_dn'high-length+1 downto 0) = zero_1_zero) or (datum_dn(datum_dn'high downto datum_dn'high-length+1) = max_pos) then
      results := datum_dn(datum_dn'high downto datum_dn'high-length+1);
    else
      results := datum_dn(datum_dn'high downto datum_dn'high-length+1) + unsigned'('0' & datum_dn(datum_dn'high-length));
    end if;
    return results;
  end function resize_even_rnd;

  function resize_ovfl (datum : signed; length : natural) return std_logic is
    variable datum_dn : signed(datum'length-1 downto 0)          := datum;
    variable zero     : signed(datum_dn'high-1 downto length -1) := (others => '0');
    variable ones     : signed(datum_dn'high-1 downto length -1) := (others => '1');
  begin           
                                      
    if datum_dn'length <= length then
      return '0';
    else
      if datum_dn(datum_dn'high) = '0' then
        if datum_dn(datum_dn'high-1 downto length-1) = zero then
          return '0';
        else
          return '1';
        end if;
      else
        if datum_dn(datum_dn'high-1 downto length-1) = ones then
          return '0';
        else
          return '1';
        end if;
      end if;
    end if; 
  end function resize_ovfl;

  function resize_sat_ovfl (overflow : std_logic; datum : signed; length : natural) return signed is
    variable datum_dn : signed(datum'length-1 downto 0) := datum;
    variable ones_ms1 : signed(length-2 downto 0)       := (others => '1');
    variable max_pos  : signed(length-1 downto 0)       := '0' & ones_ms1;
    variable zero_ms2 : signed(length-3 downto 0)       := (others => '0');
    variable max_neg  : signed(length-1 downto 0)       := '1' & zero_ms2 & '1';
    variable rst      : signed(length-1 downto 0);
  begin
    if overflow = '1' then
      if datum(datum'left) = '0' then
        rst := max_pos;
      else
        rst := max_neg;
      end if;
    else
      rst := datum_dn(length-1 downto 0);
    end if;
    return rst;
  end function resize_sat_ovfl;

end numeric_csw;
-- SNOWBALL-FILE: fixed_csw.vhd


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_float_types.all;
use ieee.fixed_pkg.all;
use work.math_csw.all;
use work.numeric_csw.all;
                         
use std.textio.all;

package fixed_csw is

  function value_max (val : sfixed) return sfixed;
  function value_max (val : ufixed) return ufixed;

  function value_min_wanted (val : sfixed) return sfixed;
  function value_min (val : sfixed) return sfixed;
  function value_min (val : ufixed) return ufixed;

  function value_zero (val : sfixed) return sfixed;
  function value_zero (val : ufixed) return ufixed;

  function to_ufixed (num: sfixed) return ufixed;

  function to_signed (num: sfixed) return signed;

  function to_unsigned (num: ufixed) return unsigned;

  function "<" (aa  : ufixed; bb : sfixed) return boolean;
  function "<=" (aa : ufixed; bb : sfixed) return boolean;
  function ">" (aa  : ufixed; bb : sfixed) return boolean;
  function ">=" (aa : ufixed; bb : sfixed) return boolean;

  function "<" (aa  : sfixed; bb : ufixed) return boolean;
  function "<=" (aa : sfixed; bb : ufixed) return boolean;
  function ">" (aa  : sfixed; bb : ufixed) return boolean;
  function ">=" (aa : sfixed; bb : ufixed) return boolean;

  function negate (num: sfixed) return sfixed;

  function negate (num: ufixed) return sfixed;

  function rmsgn (num           : ufixed;
                  warn_overflow : boolean := true;
                  saturate      : boolean := true) return ufixed;
  function rmsgn (num           : sfixed;
                  warn_overflow : boolean := true;
                  saturate      : boolean := true) return sfixed;

  function wrap_sum (val : ufixed) return ufixed;
  function wrap_sum (val : sfixed) return sfixed;

  function remove_max_neg (val : sfixed) return sfixed;

  function iszeros (num: sfixed) return boolean;

  function iszeros (num: ufixed) return boolean;

  function "*" (a: sfixed; b: std_logic) return sfixed;
  function "*" (a: std_logic; b: sfixed) return sfixed;

  function "*" (a: ufixed; b: std_logic) return ufixed;
  function "*" (a: std_logic; b: ufixed) return ufixed;

  function "*" (a: ufixed; b: sfixed) return sfixed;
  function "*" (a: sfixed; b: ufixed) return sfixed;

  function "+" (a: ufixed; b: sfixed) return sfixed;
  function "+" (a: sfixed; b: ufixed) return sfixed;
  function "-" (a: ufixed; b: sfixed) return sfixed;
  function "-" (a: sfixed; b: ufixed) return sfixed;

  function free_resize (
    val        : ufixed;
    proto      : ufixed;
    discard_ok : boolean := false)
    return ufixed;
  function free_resize (
    val        : sfixed;
    proto      : sfixed;
    discard_ok : boolean := false)
    return sfixed;

  function is_identical_range (aa, bb : sfixed) return boolean;
  function is_identical_range (aa, bb : ufixed) return boolean;

  function to_01_bitwise(vec : ufixed; xmap : std_logic := '0') return ufixed;
  function to_01_bitwise(vec : sfixed; xmap : std_logic := '0') return sfixed;
  function to_01_bw(vec : ufixed; xmap : std_logic := '0') return ufixed;
  function to_01_bw(vec : sfixed; xmap : std_logic := '0') return sfixed;

  function float_equal (aa : sfixed; bb : real; tol_lsbs : real := 1.0) return boolean;

  procedure dec_read  (ln : inout line; val : out sfixed; good : out boolean);
  procedure dec_read  (ln : inout line; val : out sfixed);
  procedure dec_read  (ln : inout line; val : out ufixed; good : out boolean);
  procedure dec_read  (ln : inout line; val : out ufixed);
  procedure dec_write (ln : inout line; val : sfixed; justified : side := right; field : in width := 0);
  procedure dec_write (ln : inout line; val : ufixed; justified : side := right; field : in width := 0);
  alias dread is dec_read [line, sfixed, boolean];
  alias dread is dec_read [line, sfixed];
  alias dread is dec_read [line, ufixed, boolean];
  alias dread is dec_read [line, ufixed];
  alias dwrite is dec_write [line, sfixed, side, width];
  alias dwrite is dec_write [line, ufixed, side, width];

  function to_dec_string (val : sfixed) return string;
  function to_dec_string (val : ufixed) return string;
  alias to_dstring is to_dec_string [ sfixed return string ];
  alias to_dstring is to_dec_string [ ufixed return string ];

end package fixed_csw;

package body fixed_csw is

  function value_max (val : sfixed) return sfixed is
    variable result : sfixed(val'range);
  begin
    result              := (others => '1');
    result(result'left) := '0';
    return result;
  end function value_max;
    
  function value_max (val : ufixed) return ufixed is
    variable result : ufixed(val'range);
  begin
    result := (others => '1');
    return result;
  end function value_max;

  function value_min_wanted (val : sfixed) return sfixed is
    variable result : sfixed(val'range);
  begin
    result               := (others => '0');
    result(result'left)  := '1';
    result(result'right) := '1';
    return result;
  end function value_min_wanted;
    
  function value_min (val : sfixed) return sfixed is
    variable result : sfixed(val'range);
  begin
    result              := (others => '0');
    result(result'left) := '1';
    return result;
  end function value_min;
    
  function value_min (val : ufixed) return ufixed is
    variable result : ufixed(val'range);
  begin
    result := (others => '0');
    return result;
  end function value_min;

  function value_zero (val : sfixed) return sfixed is
    variable result : sfixed(val'range);
  begin
    result := (others => '0');
    return result;
  end function value_zero;
    
  function value_zero (val : ufixed) return ufixed is
    variable result : ufixed(val'range);
  begin
    result := (others => '0');
    return result;
  end function value_zero;

  function to_ufixed (num: sfixed) return ufixed is
    alias num_down : sfixed(num'high downto num'low) is num;
    variable result : ufixed(num'high-1 downto num'low);
  begin
    assert not num'ascending report "SFIXED values must always have descending index ranges." severity error;

    result := ufixed(num_down(num'high-1 downto num'low));

    if (num'length > 0) and (num(num'high) = '1') then
      result := (others => '0');
    end if;
    
    return result;
  end to_ufixed;

  function to_signed (num: sfixed) return signed is
    alias result : sfixed(num'length-1 downto 0) is num;
  begin
    return signed(result);
  end to_signed;

  function to_unsigned (num: ufixed) return unsigned is
    alias result : ufixed(num'length-1 downto 0) is num;
  begin
    return unsigned(result);
  end to_unsigned;

  function "<" (aa  : ufixed; bb : sfixed) return boolean is
    variable aa_sfx : sfixed(aa'high+1 downto aa'low) := to_sfixed(aa);
  begin
    return aa_sfx < bb;
  end function "<";
    
  function "<=" (aa : ufixed; bb : sfixed) return boolean is
    variable aa_sfx : sfixed(aa'high+1 downto aa'low) := to_sfixed(aa);
  begin
    return aa_sfx <= bb;
  end function "<=";
    
  function ">" (aa  : ufixed; bb : sfixed) return boolean is
    variable aa_sfx : sfixed(aa'high+1 downto aa'low) := to_sfixed(aa);
  begin
    return aa_sfx > bb;
  end function ">";
    
  function ">=" (aa : ufixed; bb : sfixed) return boolean is
    variable aa_sfx : sfixed(aa'high+1 downto aa'low) := to_sfixed(aa);
  begin
    return aa_sfx >= bb;
  end function ">=";

  function "<" (aa  : sfixed; bb : ufixed) return boolean is
    variable bb_sfx : sfixed(bb'high+1 downto bb'low) := to_sfixed(bb);
  begin
    return aa < bb_sfx;
  end function "<";
    
  function "<=" (aa : sfixed; bb : ufixed) return boolean is
    variable bb_sfx : sfixed(bb'high+1 downto bb'low) := to_sfixed(bb);
  begin
    return aa <= bb_sfx;
  end function "<=";
    
  function ">" (aa  : sfixed; bb : ufixed) return boolean is
    variable bb_sfx : sfixed(bb'high+1 downto bb'low) := to_sfixed(bb);
  begin
    return aa > bb_sfx;
  end function ">";
    
  function ">=" (aa : sfixed; bb : ufixed) return boolean is
    variable bb_sfx : sfixed(bb'high+1 downto bb'low) := to_sfixed(bb);
  begin
    return aa >= bb_sfx;
  end function ">=";

  function negate (
    num : sfixed
  ) return sfixed is
    constant wi : natural := num'length;
    variable max_neg : sfixed(num'high downto num'low);
    variable result : sfixed(num'high downto num'low);
  begin           
                                     
    max_neg := (others => '0');
    max_neg(max_neg'high) := '1';
    if to_01_bw(num) = max_neg then
      result := (others => '1');
      result(result'high) := '0';
      return result;
    else
      return wrap_sum(-num);
    end if;
  end negate;

  function negate (
    num : ufixed
  ) return sfixed is
    variable sfx : sfixed(num'high+1 downto num'low);
  begin
    sfx := sfixed('0' & num);                                               
    return wrap_sum(-sfx);                                                 
  end negate;

  function rmsgn (
    num           : sfixed;
    warn_overflow : boolean := true;
    saturate      : boolean := true
  ) return sfixed is
    alias num_down    : sfixed(num'high downto num'low) is num;
    variable sign_bit : std_logic;
    variable result   : sfixed(num'high-1 downto num'low);
  begin          
    assert not num'ascending report "SFIXED values must always have descending index ranges." severity error;
    assert num'length > 1 report "rmsgn() requires input length larger than 1." severity warning;

    result := num_down(num_down'high-1 downto num_down'low);

    if (num_down'length > 2) then
      sign_bit := num_down(num_down'high);
      
      if sign_bit /= result(result'high) then
                           
        assert false or (not warn_overflow)
          report "Sign bit not redundant."
          severity error;

        if saturate then
          result := sign_bit & (result'high-1 downto result'low => not sign_bit);
        end if;
      end if;
    end if;

    return result;
  end rmsgn;

  function rmsgn (
    num           : ufixed;
    warn_overflow : boolean := true;
    saturate      : boolean := true
  ) return ufixed is
    alias num_down    : ufixed(num'high downto num'low) is num;
    variable result   : ufixed(num'high-1 downto num'low);
  begin          
    assert not num'ascending report "UFIXED values must always have descending index ranges." severity error;
    assert num'length > 1 report "rmsgn() requires input length larger than 1." severity warning;

    result := num_down(num_down'high-1 downto num_down'low);

    if (num_down'length > 2) then
      if is_x(num_down(num_down'high)) then
        assert false or (not warn_overflow)
          report "Metavalue encountered removing UFIXED sign bit."
          severity warning;
        
      elsif '0' /= num_down(num_down'high) then
                           
        assert false or (not warn_overflow)
          report "Sign bit not redundant."
          severity error;

        if saturate then
          result := (others => '1');
        end if;
      end if;
    end if;

    return result;
  end rmsgn;

  function wrap_sum (val : ufixed) return ufixed is
    variable val_dn : ufixed(val'left downto val'right) := val;
  begin
    assert not val'ascending report "UFIXED values must always have descending index ranges." severity error;
    return val_dn(val'high-1 downto val'low);
  end function wrap_sum;

  function wrap_sum (val : sfixed) return sfixed is
    variable val_dn : sfixed(val'left downto val'right) := val;
  begin
    assert not val'ascending report "SFIXED values must always have descending index ranges." severity error;
    return val_dn(val'high-1 downto val'low);
  end function wrap_sum;

  function remove_max_neg (val : sfixed) return sfixed is
    variable result : sfixed(val'range);
  begin
    result := sfixed(remove_max_neg(signed(to_slv(val))));
    return result;
  end function remove_max_neg;

  function iszeros (
    num: sfixed
  ) return boolean is
  begin
    return iszeros(std_logic_vector(num));
  end iszeros;

  function iszeros (
    num: ufixed
  ) return boolean is
  begin
    return iszeros(std_logic_vector(num));
  end iszeros;

  function "*" (a : sfixed; b : ufixed) return sfixed is
  begin
    return b*a;
  end "*";

  function "*" (a : ufixed; b : sfixed) return sfixed is
    variable a_signed : sfixed(a'left+1 downto a'right);
  begin
    assert not a'ascending report "Expected descending range on UFIXED input." severity failure;
    a_signed := sfixed('0' & a);
    return wrap_sum(a_signed * b);
  end "*";

  function "*" (
    a: sfixed;
    b: std_logic
  ) return sfixed is
    variable result : sfixed(a'range) := (others => '0');
  begin 
    if '1' = b then
      return a;
    else
      return result;
    end if;
  end "*";

  function "*" (a : std_logic; b : sfixed) return sfixed is
  begin
    return b*a;
  end "*";

  function "*" (
    a: ufixed;
    b: std_logic
  ) return ufixed is
    variable result : ufixed(a'range) := (others => '0');
  begin
    if '1' = b then
      return a;
    else
      return result;
    end if;
  end "*";
  
  function "*" (a : std_logic; b : ufixed) return ufixed is
  begin
    return b*a;
  end "*";

  function "+" (a: ufixed; b: sfixed) return sfixed is
  begin
    return to_sfixed(a) + b;
  end function "+";
  function "+" (a: sfixed; b: ufixed) return sfixed is
  begin
    return a + to_sfixed(b);
  end function "+";
  
  function "-" (a: ufixed; b: sfixed) return sfixed is
  begin
    return to_sfixed(a) - b;
  end function "-";
  function "-" (a: sfixed; b: ufixed) return sfixed is
  begin
    return a - to_sfixed(b);
  end function "-";

  function free_resize (
    val        : ufixed;
    proto      : ufixed;
    discard_ok : boolean := false)
    return ufixed
  is
    constant proto_zeros : ufixed(proto'range) := (others => '0');
  begin
    if val'length > 1 then
      if not discard_ok then
        assert (proto'high >= val'high) and (val'low >= proto'low)
          report "Discarding bits is not free, set 'DISCARD_OK' input to TRUE if desired."
           &" (input "&to_string(val'high)&" dt "&to_string(val'low)
           &", proto "&to_string(proto'high)&" dt "&to_string(proto'low)&")"
          severity failure;
      end if;

      return resize(val, proto,
                    round_style    => fixed_truncate,
                    overflow_style => fixed_wrap);
    else

      return proto_zeros;
    end if;
  end function free_resize;

  function free_resize (
    val        : sfixed;
    proto      : sfixed;
    discard_ok : boolean := false)
    return sfixed
  is
    constant proto_zeros : sfixed(proto'range) := (others => '0');
  begin
    if val'length > 1 then
      if not discard_ok then
        assert (proto'high >= val'high) and (val'low >= proto'low)
          report "Discarding bits is not free, set 'DISCARD_OK' input to TRUE if desired."
           &" (input "&to_string(val'high)&" dt "&to_string(val'low)
           &", proto "&to_string(proto'high)&" dt "&to_string(proto'low)&")"
          severity failure;
      end if;
      
      return resize(val, proto,
                    round_style    => fixed_truncate,
                    overflow_style => fixed_wrap);
    else

      return proto_zeros;
    end if;
  end function free_resize;

  function is_identical_range (aa, bb : sfixed) return boolean is
  begin
    return (aa'ascending = bb'ascending)
      and (aa'left = bb'left)
      and (aa'right = bb'right);
  end function is_identical_range;
  
  function is_identical_range (aa, bb : ufixed) return boolean is
  begin
    return (aa'ascending = bb'ascending)
      and (aa'left = bb'left)
      and (aa'right = bb'right);
  end function is_identical_range;

  function to_01_bitwise(vec : ufixed; xmap : std_logic := '0') return ufixed is
    variable result : ufixed(vec'range);
  begin
    result := ufixed(to_01_bitwise(to_slv(vec), xmap));
    return result;
  end function to_01_bitwise;
                                          
  function to_01_bitwise(vec : sfixed; xmap : std_logic := '0') return sfixed is
    variable result : sfixed(vec'range);
  begin
    result := sfixed(to_01_bitwise(to_slv(vec), xmap));
    return result;
  end function to_01_bitwise;

  function to_01_bw(vec : ufixed; xmap : std_logic := '0') return ufixed is
  begin
    return to_01_bitwise(vec, xmap);
  end function to_01_bw;
                                          
  function to_01_bw(vec : sfixed; xmap : std_logic := '0') return sfixed is
  begin
    return to_01_bitwise(vec, xmap);
  end function to_01_bw;

  function float_equal (aa : sfixed; bb : real; tol_lsbs : real := 1.0) return boolean is
    variable tol : real;
    variable err : real;
  begin
                             
    if (bb > to_real(value_max(aa))) and (aa = value_max(aa)) then
      return true;
    end if;
    if (bb < to_real(value_min_wanted(aa))) and (aa <= value_min_wanted(aa)) then
      return true;
    end if;

    tol := tol_lsbs * 2.0**aa'low;
    err := abs(to_real(aa) - bb);
    if (abs err) >= tol then
      err := err + 0.0;                           
    end if;
    return (abs err) < tol;
  end function float_equal;

  procedure dec_read (ln : inout line; val : out sfixed; good : out boolean) is
    variable tmp  : real;
    constant lidx : integer := val'left;
    constant ridx : integer := val'right;
  begin
    read(ln, tmp, good);
    val := to_sfixed(tmp, left_index => lidx, right_index => ridx);
  end procedure dec_read;
  
  procedure dec_read  (ln : inout line; val : out sfixed) is
    variable good : boolean;
  begin
    dec_read(ln, val, good);
  end procedure dec_read;

  procedure dec_read (ln : inout line; val : out ufixed; good : out boolean) is
    variable tmp  : real;
    constant lidx : integer := val'left;
    constant ridx : integer := val'right;
  begin
    read(ln, tmp, good);
    val := to_ufixed(tmp, left_index => lidx, right_index => ridx);
  end procedure dec_read;
  
  procedure dec_read  (ln : inout line; val : out ufixed) is
    variable good : boolean;
  begin 
      dec_read(ln, val, good);
  end procedure dec_read;

  procedure dec_write (ln : inout line; val : sfixed; justified : side := right; field : in width := 0) is
  begin
    write(ln, to_real(to_01(val)), justified, field);
  end procedure dec_write;

  procedure dec_write (ln : inout line; val : ufixed; justified : side := right; field : in width := 0) is
  begin
    write(ln, to_real(to_01(val)), justified, field);
  end procedure dec_write;

  function to_dec_string (val : sfixed) return string is
  begin
    return real'image(to_real(val));
  end function to_dec_string;
  
  function to_dec_string (val : ufixed) return string is
  begin
    return real'image(to_real(val));
  end function to_dec_string;

end package body fixed_csw;
-- SNOWBALL-FILE: vector_types_pkg.vhd


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use ieee.math_real.all;                                                                                            
use work.math_csw.all;
use work.fixed_csw.all;
                         
use std.textio.all;

package vector_types_pkg is

  type signed_vector is array (natural range <>) of signed;
  type unsigned_vector is array (natural range <>) of unsigned;
  type sfixed_vector is array (natural range <>) of sfixed;
  type ufixed_vector is array (natural range <>) of ufixed;

  function "ror" (val:sfixed_vector; shift:integer) return sfixed_vector;
  function "rol" (val:sfixed_vector; shift:integer) return sfixed_vector;
  function "ror" (val:ufixed_vector; shift:integer) return ufixed_vector;
  function "rol" (val:ufixed_vector; shift:integer) return ufixed_vector;
  function "ror" (val:unsigned_vector; shift:integer) return unsigned_vector;
  function "rol" (val:unsigned_vector; shift:integer) return unsigned_vector;
  function "ror" (val:signed_vector; shift:integer) return signed_vector;
  function "rol" (val:signed_vector; shift:integer) return signed_vector;

  procedure read  (ln : inout line; val : out sfixed_vector; good : out boolean);
  procedure read  (ln : inout line; val : out sfixed_vector);
  procedure write (ln : inout line; val : sfixed_vector; justified : side := right; field : in width := 0);
  procedure write (ln : inout line; val : ufixed_vector; justified : side := right; field : in width := 0);

  procedure dec_read  (ln : inout line; val : out sfixed_vector; good : out boolean);
  procedure dec_read  (ln : inout line; val : out sfixed_vector);
  procedure dec_write (ln : inout line; val : sfixed_vector; justified : side := right; field : in width := 0);
  procedure dec_write (ln : inout line; val : ufixed_vector; justified : side := right; field : in width := 0);
  alias dread is dec_read [line, sfixed_vector, boolean];
  alias dread is dec_read [line, sfixed_vector];
  alias dwrite is dec_write [line, sfixed_vector, side, width];
  alias dwrite is dec_write [line, ufixed_vector, side, width];

  function to_string (val : sfixed_vector) return string;
  function to_string (val : ufixed_vector) return string;
  function to_dstring (val : sfixed_vector) return string;
  function to_dstring (val : ufixed_vector) return string;

  function to_std_logic_vector (val : signed_vector) return std_logic_vector;
  function to_std_logic_vector (val : unsigned_vector) return std_logic_vector;
  function to_std_logic_vector (val : sfixed_vector) return std_logic_vector;
  function to_std_logic_vector (val : ufixed_vector) return std_logic_vector;
  function to_signed_vector (val : std_logic_vector; format : signed_vector) return signed_vector;
  function to_signed_vector (val : sfixed_vector) return signed_vector;
  function to_unsigned_vector (val : std_logic_vector; format : unsigned_vector) return unsigned_vector;
  function to_unsigned_vector (val : ufixed_vector) return unsigned_vector;
  function to_unsigned_vector (val : integer_vector; wi_data : natural) return unsigned_vector;
  function to_sfixed_vector (val : std_logic_vector; format : sfixed_vector) return sfixed_vector;
  function to_sfixed_vector (val : real_vector; format : sfixed) return sfixed_vector;
  function to_sfixed_vector (val : signed_vector; format : sfixed) return sfixed_vector;
  function to_ufixed_vector (val : std_logic_vector; format : ufixed_vector) return ufixed_vector;
  function to_ufixed_vector (val : real_vector; format : ufixed) return ufixed_vector;
  function to_ufixed_vector (val : unsigned_vector; format : ufixed) return ufixed_vector;
  function to_real (val : sfixed_vector) return real_vector;
  function to_real (val : ufixed_vector) return real_vector;
                              
  alias to_slv is to_std_logic_vector [ signed_vector return std_logic_vector ];
  alias to_slv is to_std_logic_vector [ unsigned_vector return std_logic_vector ];
  alias to_slv is to_std_logic_vector [ sfixed_vector return std_logic_vector ];
  alias to_sv is to_signed_vector [ std_logic_vector, signed_vector return signed_vector ];
  alias to_sv is to_signed_vector [ sfixed_vector return signed_vector ];
  alias to_sfxv is to_sfixed_vector [ std_logic_vector, sfixed_vector return sfixed_vector ];
                              
  function is_identical_ranges (aa, bb : sfixed_vector) return boolean;
  function is_identical_ranges (aa, bb : ufixed_vector) return boolean;

  type std_logic_2vector is array (natural range <>) of std_logic_vector;
  type signed_2vector is array (natural range <>) of signed_vector;
  type unsigned_2vector is array (natural range <>) of unsigned_vector;
  type sfixed_2vector is array (natural range <>) of sfixed_vector;
  type ufixed_2vector is array (natural range <>) of ufixed_vector;

  function "ror" (val : std_logic_2vector; shift : integer) return std_logic_2vector;
  function "rol" (val : std_logic_2vector; shift : integer) return std_logic_2vector;
  function "ror" (val : sfixed_2vector; shift : integer) return sfixed_2vector;
  function "rol" (val : sfixed_2vector; shift : integer) return sfixed_2vector;
  function "ror" (val : ufixed_2vector; shift : integer) return ufixed_2vector;
  function "rol" (val : ufixed_2vector; shift : integer) return ufixed_2vector;
  function "ror" (val : unsigned_2vector; shift : integer) return unsigned_2vector;
  function "rol" (val : unsigned_2vector; shift : integer) return unsigned_2vector;
  function "ror" (val : signed_2vector; shift : integer) return signed_2vector;
  function "rol" (val : signed_2vector; shift : integer) return signed_2vector;

  function to_std_logic_vector (val   : std_logic_2vector) return std_logic_vector;
  function to_std_logic_2vector (val  : sfixed_vector) return std_logic_2vector;
  function to_signed_2vector (val     : sfixed_2vector) return signed_2vector;
  function to_signed_2vector (input   : integer_vector; len1 : integer; len2 : integer; wi_signed : integer) return signed_2vector;
  function from_std_logic_vector (val : std_logic_vector; len1 : natural) return std_logic_2vector;
  function from_std_logic_vector (val : std_logic_vector; len1 : natural) return unsigned_vector;
  alias to_slv is to_std_logic_vector [ std_logic_2vector return std_logic_vector ];
  alias to_sl2v is to_std_logic_2vector [ sfixed_vector return std_logic_2vector ];
  alias to_s2v is to_signed_2vector [ sfixed_2vector return signed_2vector ];

  function to_hstring (val : std_logic_2vector) return string;
  function to_hstring (val : signed_vector) return string;

  function is_identical_ranges (aa, bb : sfixed_2vector) return boolean;
  function is_identical_ranges (aa, bb : ufixed_2vector) return boolean;

  function flatten (val : sfixed_2vector) return sfixed_vector;
  function flatten (val : std_logic_2vector) return std_logic_vector;
  function flatten (val : unsigned_vector) return std_logic_vector;

  function reshape (vec : sfixed_vector; len1 : integer) return sfixed_2vector;
  function reshape (val : sfixed_2vector; len1 : integer) return sfixed_2vector;

  function transpose (val : sfixed_2vector) return sfixed_2vector;

  type std_logic_3vector is array (natural range <>) of std_logic_2vector;
  type signed_3vector is array (natural range <>) of signed_2vector;
  type unsigned_3vector is array (natural range <>) of unsigned_2vector;
  type sfixed_3vector is array (natural range <>) of sfixed_2vector;
  type ufixed_3vector is array (natural range <>) of ufixed_2vector;

  function to_std_logic_3vector (val : sfixed_2vector) return std_logic_3vector;
  function to_signed_3vector (val    : sfixed_3vector) return signed_3vector;
  alias to_sl3v is to_std_logic_3vector [ sfixed_2vector return std_logic_3vector ];
  alias to_s3v is to_signed_3vector [ sfixed_3vector return signed_3vector ];
                           
  function to_hstring (val : std_logic_3vector; prefix : string := "") return string;
  function to_hstring (val : std_logic_3vector) return string;

  type std_logic_4vector is array (natural range <>) of std_logic_3vector;
  type signed_4vector is array (natural range <>) of signed_3vector;
  type unsigned_4vector is array (natural range <>) of unsigned_3vector;
  type sfixed_4vector is array (natural range <>) of sfixed_3vector;
  type ufixed_4vector is array (natural range <>) of ufixed_3vector;
                           
  function to_hstring (val : std_logic_4vector) return string;

  function resize (val : signed_vector; length : positive) return signed_vector;
  function resize (val : signed_vector; format : signed_vector) return signed_vector;
  function resize (val : unsigned_vector; length : positive) return unsigned_vector;
  function resize (val : unsigned_vector; format : unsigned_vector) return unsigned_vector;

  function free_resize (                                        
    val        : sfixed_vector;
    format     : sfixed;
    discard_ok : boolean := false)
    return sfixed_vector;

  function free_resize (
    val        : sfixed_vector;
    lindex     : integer;
    rindex     : integer;
    discard_ok : boolean := false)
    return sfixed_vector;

  function free_resize (                                        
    val        : ufixed_vector;
    format     : ufixed;
    discard_ok : boolean := false)
    return ufixed_vector;

  function free_resize (
    val        : ufixed_vector;
    lindex     : integer;
    rindex     : integer;
    discard_ok : boolean := false)
    return ufixed_vector;

  function shift_left (val : signed_vector; sh : integer) return signed_vector;
  function shift_right (val : signed_vector; sh : integer) return signed_vector;
  function "sra" (val : signed_vector; sh : integer) return signed_vector;
  function "sla" (val : signed_vector; sh : integer) return signed_vector;

end package vector_types_pkg;

package body vector_types_pkg is

  function "ror" (val : sfixed_vector; shift : integer) return sfixed_vector is
    variable result  : sfixed_vector(val'range)(val'element'range);
    constant len     : natural := val'length;
    subtype curr_t is sfixed_vector(0 to len-1)(val'element'range);

    variable val_up : curr_t;
    variable res_up : curr_t;
    constant shift_i : integer := shift mod len;
  begin
    if 0 = shift_i then
      return val;
    else

      val_up := val;
      for ii in shift_i to len-1 loop
        res_up(ii) := val_up(ii + 0-shift_i);
      end loop;
      for ii in 0 to shift_i-1 loop
        res_up(ii) := val_up(ii + len-shift_i);
      end loop;
      result := res_up;
      return result;
    end if;
  end function "ror";

  function "rol" (val : sfixed_vector; shift : integer) return sfixed_vector is
    variable result : sfixed_vector(val'range)(val'element'range);
    subtype curr_t is sfixed_vector(0 to val'length-1)(val'element'range);
    alias val_up    : curr_t is val;
    alias res_up    : curr_t is result;
    constant shift_i   : integer := shift mod val'length;
  begin
    if 0 = shift_i then
      return val;
    else
      res_up(0 to res_up'right-shift_i)             := val_up(shift_i to val_up'right);
      res_up(res_up'length-shift_i to res_up'right) := val_up(0 to shift_i-1);
      return result;
    end if;
  end function "rol";

  function "ror" (val : ufixed_vector; shift : integer) return ufixed_vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
                                                                                    
    variable ror_sfxv : val_sfxv'subtype;
    variable res_ufxv : val'subtype;
  begin
    ror_sfxv := val_sfxv ror shift;
    res_ufxv := ufixed_vector(ror_sfxv);
    return res_ufxv;
  end function "ror";

  function "rol" (val : ufixed_vector; shift : integer) return ufixed_vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
  begin
    return ufixed_vector(val_sfxv rol shift);
  end function "rol";

  function "ror" (val : std_logic_2vector; shift : integer) return std_logic_2vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
  begin
    return std_logic_2vector(val_sfxv ror shift);
  end function "ror";

  function "rol" (val : std_logic_2vector; shift : integer) return std_logic_2vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
  begin
    return std_logic_2vector(val_sfxv rol shift);
  end function "rol";

  function "ror" (val:unsigned_vector; shift:integer) return unsigned_vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
  begin
    return unsigned_vector(val_sfxv ror shift);
  end function "ror";

  function "rol" (val:unsigned_vector; shift:integer) return unsigned_vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
  begin
    return unsigned_vector(val_sfxv rol shift);
  end function "rol";

  function "ror" (val:signed_vector; shift:integer) return signed_vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
  begin
    return signed_vector(val_sfxv ror shift);
  end function "ror";

  function "rol" (val:signed_vector; shift:integer) return signed_vector is
    constant val_sfxv : sfixed_vector(val'range)(val'element'range) := sfixed_vector(val);
  begin
    return signed_vector(val_sfxv rol shift);
  end function "rol";

  function "ror" (val : sfixed_2vector; shift : integer) return sfixed_2vector is
    variable result  : sfixed_2vector(val'range)(val'element'range)(val'element'element'range);
    constant len     : natural := val'length;
    subtype curr_t is sfixed_2vector(0 to len-1)(val'element'range)(val'element'element'range);
    variable val_up : curr_t;
    variable res_up : curr_t;
    constant shift_i : integer := shift mod len;
  begin
    if 0 = shift_i then
      return val;
    else
      val_up := val;
      for ii in shift_i to len-1 loop
        res_up(ii) := val_up(ii + 0-shift_i);
      end loop;
      for ii in 0 to shift_i-1 loop
        res_up(ii) := val_up(ii + len-shift_i);
      end loop;
      result := res_up;
      return result;
    end if;
  end function "ror";

  function "rol" (val : sfixed_2vector; shift : integer) return sfixed_2vector is
  begin
    return val ror -shift;
  end function "rol";

  function "ror" (val : ufixed_2vector; shift : integer) return ufixed_2vector is
    constant val_sfx2v : sfixed_2vector(val'range)(val'element'range)(val'element'element'range)
      := sfixed_2vector(val);
  begin
    return ufixed_2vector(val_sfx2v ror shift);
  end function "ror";

  function "rol" (val : ufixed_2vector; shift : integer) return ufixed_2vector is
    constant val_sfx2v : sfixed_2vector(val'range)(val'element'range)(val'element'element'range)
      := sfixed_2vector(val);
  begin
    return ufixed_2vector(val_sfx2v rol shift);
  end function "rol";

  function "ror" (val : unsigned_2vector; shift : integer) return unsigned_2vector is
    constant val_sfx2v : sfixed_2vector(val'range)(val'element'range)(val'element'element'range)
      := sfixed_2vector(val);
  begin
    return unsigned_2vector(val_sfx2v ror shift);
  end function "ror";

  function "rol" (val : unsigned_2vector; shift : integer) return unsigned_2vector is
    constant val_sfx2v : sfixed_2vector(val'range)(val'element'range)(val'element'element'range)
      := sfixed_2vector(val);
  begin
    return unsigned_2vector(val_sfx2v rol shift);
  end function "rol";

  function "ror" (val : signed_2vector; shift : integer) return signed_2vector is
    constant val_sfx2v : sfixed_2vector(val'range)(val'element'range)(val'element'element'range)
      := sfixed_2vector(val);
  begin
    return signed_2vector(val_sfx2v ror shift);
  end function "ror";

  function "rol" (val : signed_2vector; shift : integer) return signed_2vector is
    constant val_sfx2v : sfixed_2vector(val'range)(val'element'range)(val'element'element'range)
      := sfixed_2vector(val);
  begin
    return signed_2vector(val_sfx2v rol shift);
  end function "rol";

  procedure read (ln : inout line; val : out sfixed_vector; good : out boolean) is
    variable good_i : boolean := true;
  begin
    for ii in val'range loop
      read(ln, val(ii), good_i);
      if not good_i then exit; end if;
    end loop;
    good := good_i;
  end procedure read;

  procedure read (ln : inout line; val : out sfixed_vector) is
    variable good : boolean;
  begin
    read(ln, val, good);
  end procedure read;

  procedure write (ln        : inout line;
                   val       :       sfixed_vector;
                   justified :       side  := right;
                   field     : in    width := 0)
  is
  begin
    swrite(ln, "(");
    for ii in val'range loop
      write(ln, val(ii), justified, field);
      swrite(ln, ", ");
    end loop;
    swrite(ln, ")");
  end procedure write;

  procedure write (ln        : inout line;
                   val       :       ufixed_vector;
                   justified :       side  := right;
                   field     : in    width := 0)
  is
  begin
    swrite(ln, "(");
    for ii in val'range loop
      write(ln, val(ii), justified, field);
      swrite(ln, ", ");
    end loop;
    swrite(ln, ")");
  end procedure write;

  procedure dec_read (ln : inout line; val : out sfixed_vector; good : out boolean) is
    variable good_i : boolean := true;
  begin
    for ii in val'range loop
      dec_read(ln, val(ii), good_i);
      if not good_i then exit; end if;
    end loop;
    good := good_i;
  end procedure dec_read;

  procedure dec_read (ln : inout line; val : out sfixed_vector) is
    variable good : boolean;
  begin
    for ii in val'range loop
      dec_read(ln, val(ii), good);
      if not good then
        report "Failed to read sfixed component." severity error;
        exit;
      end if;
    end loop;
  end procedure dec_read;

  procedure dec_write (ln : inout line; val : sfixed_vector; justified : side := right; field : in width := 0) is
  begin
    swrite(ln, "(");
    for ii in val'range loop
      dec_write(ln, val(ii), justified, field);
      swrite(ln, ", ");
    end loop;
    swrite(ln, ")");
  end procedure dec_write;

  procedure dec_write (ln : inout line; val : ufixed_vector; justified : side := right; field : in width := 0) is
  begin
    swrite(ln, "(");
    for ii in val'range loop
      dec_write(ln, val(ii), justified, field);
      swrite(ln, ", ");
    end loop;
    swrite(ln, ")");
  end procedure dec_write;

  function to_string (val : sfixed_vector) return string is
    variable ln : line;
  begin
    write(ln, val);
    return ln.all;
  end function to_string;
                      
  function to_string (val : ufixed_vector) return string is
    variable ln : line;
  begin
    write(ln, val);
    return ln.all;
  end function to_string;
                      
  function to_dstring (val : sfixed_vector) return string is
    variable ln : line;
  begin
    dec_write(ln, val);
    return ln.all;
  end function to_dstring;
                      
  function to_dstring (val : ufixed_vector) return string is
    variable ln : line;
  begin
    dec_write(ln, val);
    return ln.all;
  end function to_dstring;

  function to_hstring (val : std_logic_2vector) return string is
    variable result : line;
  begin
    swrite(result, "(");
    for ii in val'range loop
      write(result, to_hstring(val(ii)));
      if ii /= val'right then
        swrite(result, ", ");
      end if;
    end loop;
    swrite(result, ")");
    return result.all;
  end function to_hstring;

  function to_hstring (val : std_logic_3vector) return string is
    variable result : line;
  begin
    return to_hstring (val, "");
  end function to_hstring;

  function to_hstring (val    : std_logic_3vector;
                       prefix : string := "")
    return string is
    variable result       : line;

    constant index_string : string  := prefix & to_string(val'high);
    constant index_len    : natural := index_string'length;
  begin
    for ii in val'range loop
      write(result, prefix & to_string(ii), right, index_len);
      write(result, " => " & to_hstring(val(ii)));
      if ii /= val'right then
        write(result, "," & lf);
      end if;
    end loop;
    return result.all;
  end function to_hstring;

  function to_hstring (val : std_logic_4vector)
    return string is
    variable result : line;
  begin
    for ii in val'range loop
      write(result, to_string(ii) & " => " & lf);
      write(result, to_hstring(val(ii), "  "));
      if ii /= val'right then
        write(result, "," & lf);
      else
        write(result, lf);
      end if;
    end loop;
    return result.all;
  end function to_hstring;

  function to_hstring (val : signed_vector)
    return string is
  begin
    return to_hstring(std_logic_2vector(val));
  end function to_hstring;

  function to_std_logic_vector (val : signed_vector) return std_logic_vector is
    variable idx    : natural;
    variable val_i  : signed_vector(0 to val'length-1)(0 to val'element'length-1);
    variable result : std_logic_vector(0 to val'length * val'element'length - 1) := (others => '1');
  begin
    val_i := val;
    idx   := 0;
    for ii in val_i'range loop
      for jj in val_i'element'range loop
        result(idx) := val_i(ii)(jj);
        idx         := idx + 1;
      end loop;
    end loop;
    return result;
  end function;
                                                                        
  function to_std_logic_vector (val : unsigned_vector) return std_logic_vector is
    variable idx    : natural;
    variable val_i  : unsigned_vector(0 to val'length-1)(0 to val'element'length-1);
    variable result : std_logic_vector(0 to val'length * val'element'length - 1) := (others => '1');
  begin
    val_i := val;
    idx   := 0;
    for ii in val_i'range loop
      for jj in val_i'element'range loop
        result(idx) := val_i(ii)(jj);
        idx         := idx + 1;
      end loop;
    end loop;
    return result;
  end function;
                                                                        
  function to_std_logic_vector (val : sfixed_vector) return std_logic_vector is
    variable idx    : natural;
    variable val_i  : sfixed_vector(0 to val'length-1)(0 to val'element'length-1);
    variable result : std_logic_vector(0 to val'length * val'element'length - 1) := (others => '1');
  begin
    val_i := val;
    idx   := 0;
    for ii in val_i'range loop
      for jj in val_i'element'range loop
        result(idx) := val_i(ii)(jj);
        idx         := idx + 1;
      end loop;
    end loop;
    return result;
  end function;
                                                                        
  function to_std_logic_vector (val : ufixed_vector) return std_logic_vector is
  begin
    return to_std_logic_vector(sfixed_vector(val));
  end function;

  function to_signed_vector (val : std_logic_vector; format : signed_vector) return signed_vector is
    variable idx      : natural;
    variable val_i    : std_logic_vector(0 to val'length-1);
    variable result_i : signed_vector(0 to format'length-1)(0 to format'element'length-1);
    variable result   : format'subtype;                                                                                     
  begin
    val_i := val;
    idx   := 0;
    for ii in result_i'range loop
      for jj in result_i'element'range loop
        result_i(ii)(jj) := val_i(idx);
        idx              := idx + 1;
      end loop;
    end loop;
    result := result_i;
    return result;
  end function;

  function to_signed_vector (val : sfixed_vector) return signed_vector is
    variable result : signed_vector(val'range)(val'element'length-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := signed(to_slv(val(ii)));
    end loop;
    return result;
  end function to_signed_vector;

  function to_unsigned_vector (
    val    : std_logic_vector;
    format : unsigned_vector)
    return unsigned_vector
  is
    variable sgn_vec : signed_vector(format'range)(format(format'left)'range);
    variable result  : unsigned_vector(format'range)(format(format'left)'range);
  begin
    sgn_vec := to_signed_vector(val, sgn_vec);                              
    for ii in format'range loop
      result(ii) := unsigned(sgn_vec(ii));
    end loop;                                                                  
    return result;
  end function to_unsigned_vector;

  function to_unsigned_vector (val : ufixed_vector) return unsigned_vector is
    constant wi_data : natural := val(val'left)'length;
    variable result : unsigned_vector(val'range)(wi_data-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := unsigned(to_slv(val(ii)));
    end loop;
    return result;
  end function to_unsigned_vector;

  function to_unsigned_vector (val : integer_vector; wi_data : natural) return unsigned_vector is
    variable result  : unsigned_vector(val'range)(wi_data-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := to_unsigned(val(ii), wi_data);
    end loop;
    return result;
  end function to_unsigned_vector;

  function to_sfixed_vector (val : std_logic_vector; format : sfixed_vector) return sfixed_vector is
    constant elem_proto : format'element := (others => '0');
    constant wi_elem    : natural        := elem_proto'length;
    constant num_elem   : natural        := format'length;

    constant val_up : std_logic_vector(0 to val'length-1) := val;
    variable norm   : sfixed_vector(0 to num_elem-1)(elem_proto'range);
    variable result : format'subtype;
  begin
    assert val'length = wi_elem*num_elem
      report "Input vector size doesn't match format size."
      severity failure;

    for ii in 0 to num_elem-1 loop
      norm(ii) := sfixed(val_up(ii*wi_elem to (ii+1)*wi_elem-1));
    end loop;
    result := norm;
    return result;
  end function;

  function to_sfixed_vector (val : real_vector; format : sfixed) return sfixed_vector is
    variable result : sfixed_vector(val'range)(format'range);
  begin
    for ii in val'range loop
      result(ii) := to_sfixed(val(ii), format);
    end loop;
    return result;
  end function to_sfixed_vector;

  function to_sfixed_vector (val : signed_vector; format : sfixed) return sfixed_vector is
    variable result : sfixed_vector(val'range)(format'range);
  begin
    for ii in val'range loop
      result(ii) := sfixed(val(ii));
    end loop;
    return result;
  end function to_sfixed_vector;

  function to_ufixed_vector (val : std_logic_vector; format : ufixed_vector) return ufixed_vector is
    constant elem_proto : format'element := (others => '0');
    constant wi_elem    : natural        := elem_proto'length;
    constant num_elem   : natural        := format'length;

    constant val_up : std_logic_vector(0 to val'length-1) := val;
    variable norm   : ufixed_vector(0 to num_elem-1)(elem_proto'range);
    variable result : format'subtype;
  begin
    assert val'length = wi_elem*num_elem
      report "Input vector size doesn't match format size."
      severity failure;

    for ii in 0 to num_elem-1 loop
      norm(ii) := ufixed(val_up(ii*wi_elem to (ii+1)*wi_elem-1));
    end loop;
    result := norm;
    return result;
  end function to_ufixed_vector;

  function to_ufixed_vector (val : real_vector; format : ufixed) return ufixed_vector is
    variable result : ufixed_vector(val'range)(format'range);
  begin
    for ii in val'range loop
      result(ii) := to_ufixed(val(ii), format);
    end loop;
    return result;
  end function to_ufixed_vector;

  function to_ufixed_vector (val : unsigned_vector; format : ufixed) return ufixed_vector is
    variable result : ufixed_vector(val'range)(format'range);
  begin
    for ii in val'range loop
      result(ii) := ufixed(val(ii));
    end loop;
    return result;
  end function to_ufixed_vector;

  function to_real (val : sfixed_vector) return real_vector is
    variable result : real_vector(val'range);
  begin
    for ii in result'range loop
      result(ii) := to_real(val(ii));
    end loop;
    return result;
  end function to_real;

  function to_real (val : ufixed_vector) return real_vector is
    variable result : real_vector(val'range);
  begin
    for ii in result'range loop
      result(ii) := to_real(val(ii));
    end loop;
    return result;
  end function to_real;

  function is_identical_ranges (aa, bb : sfixed_vector) return boolean is
    constant aa_elem : aa'element := (others => '0');
    constant bb_elem : bb'element := (others => '0');
  begin
    return (aa'ascending = bb'ascending)
      and (aa'left = bb'left)
      and (aa'right = bb'right)
      and is_identical_range(aa_elem, bb_elem);
  end function is_identical_ranges;

  function is_identical_ranges (aa, bb : ufixed_vector) return boolean is
    constant aa_elem : aa'element := (others => '0');
    constant bb_elem : bb'element := (others => '0');
  begin
    return (aa'ascending = bb'ascending)
      and (aa'left = bb'left)
      and (aa'right = bb'right)
      and is_identical_range(aa_elem, bb_elem);
  end function is_identical_ranges;

  function to_std_logic_2vector (val : sfixed_vector) return std_logic_2vector is
    variable result : std_logic_2vector(val'range)(val'element'length-1 downto 0);
  begin
    assert not val'element'ascending report "Ascending sfixed range not supported." severity failure;
    for ii in val'range loop
      result(ii) := to_slv(val(ii));
    end loop;
    return result;
  end function to_std_logic_2vector;
            
  function to_signed_2vector (val : sfixed_2vector) return signed_2vector is
    variable result : signed_2vector(val'range)(val'element'range)(val'element'element'length-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := to_signed_vector(val(ii));
    end loop;
    return result;
  end function to_signed_2vector;
            
  function to_signed_2vector (input : integer_vector; len1 : integer; len2 : integer; wi_signed : integer)
    return signed_2vector is
    variable result : signed_2vector(0 to len1 - 1)(0 to len2 - 1)(wi_signed - 1 downto 0);
  begin
    assert len2 * len1 = input'length report "Input length must equal product of dimensions." severity failure;
    for ii in 0 to len1 - 1 loop
      for jj in 0 to len2 - 1 loop
        result(ii)(jj) := to_signed(input(jj + len2*ii), wi_signed);
      end loop;
    end loop;
    return result;
  end function to_signed_2vector;

  function to_std_logic_3vector (val : sfixed_2vector) return std_logic_3vector is
    variable result : std_logic_3vector(val'range)(val'element'range)(val'element'element'length-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := to_std_logic_2vector(val(ii));
    end loop;
    return result;
  end function to_std_logic_3vector;
            
  function to_signed_3vector (val : sfixed_3vector) return signed_3vector is
    variable result : signed_3vector(val'range)(val'element'range)(val'element'element'range)(val'element'element'element'length-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := to_signed_2vector(val(ii));
    end loop;
    return result;
  end function to_signed_3vector;

  function to_std_logic_vector (val : std_logic_2vector) return std_logic_vector is
    constant nelems : integer := val'length * val'element'length;
    variable idx    : natural;
    variable vec    : std_logic_vector(0 to nelems-1);
  begin
    idx := 0;
    for ii in val'range loop
      for jj in val'element'range loop
        vec(idx) := val(ii)(jj);
        idx := idx + 1;
      end loop;
    end loop;
    return vec;
  end function to_std_logic_vector;

  function from_std_logic_vector (val : std_logic_vector; len1 : natural) return std_logic_2vector is
    constant nelems     : integer := val'length;
    constant len2       : integer := nelems / len1;
    variable idx1, idx2 : natural;
    variable result     : std_logic_2vector(0 to len1-1)(0 to len2-1);
  begin
    assert len2 * len1 = nelems report "Vector count must divide bit count." severity failure;
    idx1 := 0;
    idx2 := 0;
    for ii in val'range loop
      result(idx1)(idx2) := val(ii);
      idx2 := idx2 + 1;
      if idx2 = len2 then
        idx1 := idx1 + 1;
        idx2 := 0;
      end if;
    end loop;
    return result;
  end function from_std_logic_vector;
                                          
  function from_std_logic_vector (val : std_logic_vector; len1 : natural) return unsigned_vector is
    constant nelems     : integer := val'length;
    constant len2       : integer := nelems / len1;
    variable idx1, idx2 : natural;
    variable result     : unsigned_vector(0 to len1-1)(0 to len2-1);
  begin
    assert len2 * len1 = nelems report "Vector count must divide bit count." severity failure;
    idx1 := 0;
    idx2 := 0;
    for ii in val'range loop
      result(idx1)(idx2) := val(ii);
      idx2 := idx2 + 1;
      if idx2 = len2 then
        idx1 := idx1 + 1;
        idx2 := 0;
      end if;
    end loop;
    return result;
  end function from_std_logic_vector;

  function is_identical_ranges (aa, bb : sfixed_2vector) return boolean is
    variable aa_elem : aa'element;
    variable bb_elem : bb'element;
  begin
    return (aa'ascending = bb'ascending)
      and (aa'left = bb'left)
      and (aa'right = bb'right)
      and is_identical_ranges(aa_elem, bb_elem);
  end function is_identical_ranges;

  function is_identical_ranges (aa, bb : ufixed_2vector) return boolean is
    variable aa_elem : aa'element;
    variable bb_elem : bb'element;
  begin
    return (aa'ascending = bb'ascending)
      and (aa'left = bb'left)
      and (aa'right = bb'right)
      and is_identical_ranges(aa_elem, bb_elem);
  end function is_identical_ranges;

  function flatten (val : sfixed_2vector) return sfixed_vector is
    constant nelems : integer := val'length * val'element'length;
    variable idx    : natural;
    variable vec    : sfixed_vector(0 to nelems-1)(val'element'element'range);
  begin
    idx := 0;
    for ii in val'range loop
      for jj in val'element'range loop
        vec(idx) := val(ii)(jj);
        idx      := idx + 1;
      end loop;
    end loop;
    return vec;
  end function flatten;

  function flatten (val : std_logic_2vector) return std_logic_vector is
  begin
    return to_std_logic_vector(val);
  end function flatten;

  function flatten (val : unsigned_vector) return std_logic_vector is
  begin
    return to_std_logic_vector(val);
  end function flatten;

  function reshape (vec : sfixed_vector; len1 : integer) return sfixed_2vector is
    constant nelems : integer := vec'length;
    constant len2   : integer := nelems / len1;
                                                                                   
    variable idx    : natural;
    variable inc    : integer;
    variable result : sfixed_2vector(0 to len1-1)(0 to len2-1)(vec'element'range);
  begin
                             
    assert len1 * len2 = nelems
      report "Input has "&integer'image(nelems)
            &" elements, which doesn't divide evenly by requested top length "
            &integer'image(len1)
      severity failure;

    if vec'ascending then
      inc := 1;
    else
      inc := -1;
    end if;

    idx := vec'left;
    for ii in 0 to len1-1 loop
      for jj in 0 to len2-1 loop
                                            
        result(ii)(jj) := vec(idx);
        idx            := idx + inc;
      end loop;
    end loop;
    return result;
  end function reshape;

  function reshape (val : sfixed_2vector; len1 : integer) return sfixed_2vector is
    constant vec : sfixed_vector := flatten(val);
  begin
    return reshape(vec, len1);
  end function reshape;

  function transpose (val : sfixed_2vector) return sfixed_2vector is
    variable newval : sfixed_2vector(val'element'range)(val'range)(val'element'element'range);
  begin
    for ii in val'range loop
      for jj in val'element'range loop
        newval(jj)(ii) := val(ii)(jj);
      end loop;
    end loop;
    return newval;
  end function transpose;

  function resize (val : signed_vector; length : positive) return signed_vector is
    variable result : signed_vector(val'range)(length-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := resize(val(ii), length);
    end loop;
    return result;
  end function resize;

  function resize (val : signed_vector; format : signed_vector) return signed_vector is
    variable result : signed_vector(val'range)(format'element'range);
  begin
    assert val'length = format'length and val'left = format'left and val'right = format'right
      report "Expected identical upper vector range."
      severity failure;
    for ii in val'range loop
      result(ii) := resize(val(ii), format'element'length);
    end loop;
    return result;
  end function resize;

  function resize (val : unsigned_vector; length : positive) return unsigned_vector is
    variable result : unsigned_vector(val'range)(length-1 downto 0);
  begin
    for ii in val'range loop
      result(ii) := resize(val(ii), length);
    end loop;
    return result;
  end function resize;

  function resize (val : unsigned_vector; format : unsigned_vector) return unsigned_vector is
    variable result : unsigned_vector(val'range)(format'element'range);
  begin
    assert val'length = format'length and val'left = format'left and val'right = format'right
      report "Expected identical upper vector range."
      severity failure;
    for ii in val'range loop
      result(ii) := resize(val(ii), format'element'length);
    end loop;
    return result;
  end function resize;

  function free_resize (
    val        : sfixed_vector;
    format     : sfixed;
    discard_ok : boolean := false)
    return sfixed_vector
  is
    variable result : sfixed_vector(val'range)(format'range);
  begin
    for ii in val'range loop
      result(ii) := free_resize(val(ii), format, discard_ok);
    end loop;
    return result;
  end function free_resize;

  function free_resize (
    val        : sfixed_vector;
    lindex     : integer;
    rindex     : integer;
    discard_ok : boolean := false)
    return sfixed_vector
  is
    constant format : sfixed(lindex downto rindex) := (others => '0');
  begin
    return free_resize(val, format, discard_ok);
  end function free_resize;

  function free_resize (
    val        : ufixed_vector;
    format     : ufixed;
    discard_ok : boolean := false)
    return ufixed_vector
  is
    variable result : ufixed_vector(val'range)(format'range);
  begin
    for ii in val'range loop
      result(ii) := free_resize(val(ii), format, discard_ok);
    end loop;
    return result;
  end function free_resize;

  function free_resize (
    val        : ufixed_vector;
    lindex     : integer;
    rindex     : integer;
    discard_ok : boolean := false)
    return ufixed_vector
  is
    constant format : ufixed(lindex downto rindex) := (others => '0');
  begin
    return free_resize(val, format, discard_ok);
  end function free_resize;

  function shift_left (val : signed_vector; sh : integer) return signed_vector is
    variable result : signed_vector(val'range)(val'element'range);
  begin
    for ii in val'range loop
      result(ii) := shift_left(val(ii), sh);
    end loop;
    return result;
  end function;

  function shift_right (val : signed_vector; sh : integer) return signed_vector is
    variable result : signed_vector(val'range)(val'element'range);
  begin
    for ii in val'range loop
      result(ii) := shift_right(val(ii), sh);
    end loop;
    return result;
  end function;

  function "sla" (val : signed_vector; sh : integer) return signed_vector is
  begin
    return shift_left(val, sh);
  end function;

  function "sra" (val : signed_vector; sh : integer) return signed_vector is
  begin
    return shift_right(val, sh);
  end function;

end package body vector_types_pkg;
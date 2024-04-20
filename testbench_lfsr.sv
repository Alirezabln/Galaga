module testbench_lfsr();
  logic        	clk, reset;
  logic 		number_catch;
  logic 		[9:0] seed;
  logic 		[9:0] RNG;
   
  // instantiate device under test
  lfsr dut(clk, reset, number_catch, seed, RNG);
  
  // generate clock
  always     // no sensitivity list, so it always executes
    begin
      clk = 1'b1; #5; clk = 1'b0; #5;
    end

  // at start of test, load vectors and pulse reset 
  // then alternately set up and down
  initial
    begin
	  seed = 10'd1; number_catch = 1'b0; // Initialize inputs
      reset = 1'b1; #1; reset = 1'b0; // reset
	  #10 // load the 1 seed
	  number_catch = 1'b1;  
	  for (int i = 0; i < 16; i++) begin
		  #2; number_catch = 1'b0;
		  #10; number_catch = 1'b1; 
	  end 
	  number_catch = 1'b0;
	  seed = 10'd50; reset = 1'b1; #10 reset = 1'b0; // load 2 seed
	  #10
	  number_catch = 1'b1;
	  for (int i = 0; i < 16; i++) begin
		  #2; number_catch = 1'b0;
		  #10; number_catch = 1'b1; 
	  end 
    end

endmodule
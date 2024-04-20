module lfsr (input logic clk, reset,
				input logic number_catch,
				input logic [9:0] seed,
				output logic [9:0] RNG);
				
	logic [9:0] count;
	
	always_ff @(posedge clk, posedge reset) begin
		if (reset)				count <= seed;
		else 					count <= {count[0] ^ count[9], count[9:1]};
	end 
		
	always_ff @(posedge number_catch, posedge reset) begin
		if (reset) 				RNG <= 10'd0;
		else 				    RNG <= count % 10'd641;
	end
	
endmodule
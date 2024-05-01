// spaceDash.sv
// Alireza Boloutian

module spaceDash(input  logic clk, reset,
				   input  logic keyright, keyleft, keyup, keydown, 
				   input  logic [9:0] seed,
				   output logic vgaclk,          // 25.175 MHz VGA clock 
				   output logic hsync, vsync, 
				   output logic sync_b, blank_b, // to monitor & DAC 
				   output logic [7:0] r, g, b,
				   output logic [6:0] segments0,
				   output logic [6:0] segments1,
				   output logic [6:0] segments2,
				   output logic [6:0] segments3);  // to video DAC 

  logic [9:0] x, y;
  logic [3:0] scoreclk;
  logic gameOver, start;
	
  // divide 50 MHz input clock by 2 to get 25 MHz clock
  always_ff @(posedge clk, posedge reset)
    if (reset)
	   vgaclk = 1'b0;
    else
	   vgaclk = ~vgaclk;
		
  // generate monitor timing signals 
  vgaController vgaCont(vgaclk, reset, hsync, vsync, sync_b, blank_b, x, y); 

  // module to control the video output
  videoGen videoGen(x, y, seed, reset, vgaclk, vsync, keyright, keyleft, keyup, keydown, start, gameOver,  r, g, b);

  // generate clock for score
  clock #(4) clock0(reset, vsync, start,  scoreclk);

  // module to control the score
  score score1(reset, scoreclk, gameOver, segments0, segments1, segments2, segments3);

  
endmodule 


module vgaController #(parameter HBP     = 10'd48,   // horizontal back porch
                                 HACTIVE = 10'd640,  // number of pixels per line
                                 HFP     = 10'd16,   // horizontal front porch
                                 HSYN    = 10'd96,   // horizontal sync pulse = 60 to move electron gun back to left
                                 HMAX    = HBP + HACTIVE + HFP + HSYN, //48+640+16+96=800: number of horizontal pixels (i.e., clock cycles)
                                 VBP     = 10'd32,   // vertical back porch
                                 VACTIVE = 10'd480,  // number of lines
                                 VFP     = 10'd11,   // vertical front porch
                                 VSYN    = 10'd2,    // vertical sync pulse = 2 to move electron gun back to top
                                 VMAX    = VBP + VACTIVE + VFP  + VSYN) //32+480+11+2=525: number of vertical pixels (i.e., clock cycles)                      

     (input  logic vgaclk, reset,
      output logic hsync, vsync, sync_b, blank_b, 
      output logic [9:0] hcnt, vcnt); 

      // counters for horizontal and vertical positions 
      always @(posedge vgaclk, posedge reset) begin 
        if (reset) begin
          hcnt <= 0;
          vcnt <= 0;
        end
        else  begin
          hcnt++; 
      	   if (hcnt == HMAX) begin 
            hcnt <= 0; 
  	        vcnt++; 
  	        if (vcnt == VMAX) 
  	          vcnt <= 0; 
          end 
        end
      end 
	  

      // compute sync signals (active low) 
      assign hsync  = ~( (hcnt >= (HACTIVE + HFP)) & (hcnt < (HACTIVE + HFP + HSYN)) ); 
      assign vsync  = ~( (vcnt >= (VACTIVE + VFP)) & (vcnt < (VACTIVE + VFP + VSYN)) ); 
      // assign sync_b = hsync & vsync; 
      assign sync_b = 1'b0;  // this should be 0 for newer monitors

      // force outputs to black when not writing pixels
      // The following also works: assign blank_b = hsync & vsync; 
      assign blank_b = (hcnt < HACTIVE) & (vcnt < VACTIVE); 
endmodule 


module videoGen(input logic [9:0] x, y, seed,
					input logic reset, vgaclk, vsync, keyright, keyleft, keyup, keydown, 
					output logic start, gameOver,
					output logic [7:0] r, g, b); 

  logic rpixel, a1pixel, a2pixel, a3pixel, a4pixel, a5pixel, a6pixel, a7pixel, spawnclk, speedclk;
  logic [6:0] en;
  logic [9:0] RNGPos, speed;
  
    // Game Over State
  gameState state1(reset, vgaclk, keyright, keyleft, keyup, keydown,  
						rpixel, a1pixel, a2pixel, a3pixel, a4pixel, a5pixel, a6pixel, a7pixel, start, gameOver);
  
  // Generate number for asteroid position
   lfsr lfsr1(reset, vsync, seed, RNGPos);

   // Clocks for asteroid spawn and speed
   clock #(8) clockSpawn(reset, vsync, start, spawnclk);
   clock #(11) clockSpeed(reset, vsync, start, speedclk);

   // Calculate asteroid speed based on speedclock
   asteroidSpeed speed1(reset, speedclk, speed);

   // Generate asteroids based on spawnclock
   asteroidEnable asteroidEnable1(reset, spawnclk, en);

   // Instantiate rocket and asteroids
  rocket r1(x, y, reset, vsync,  keyright, keyleft, keyup, keydown, gameOver, rpixel);
  asteroid a1(x, y, RNGPos, speed, reset, vsync, en[0], gameOver, a1pixel);
  asteroid a2(x, y, RNGPos, speed, reset, vsync, en[1], gameOver, a2pixel);
  asteroid a3(x, y, RNGPos, speed, reset, vsync, en[2], gameOver, a3pixel);
  asteroid a4(x, y, RNGPos, speed, reset, vsync, en[3], gameOver, a4pixel);
  asteroid a5(x, y, RNGPos, speed, reset, vsync, en[4], gameOver, a5pixel);
  asteroid a6(x, y, RNGPos, speed, reset, vsync, en[5], gameOver, a6pixel);
  asteroid a7(x, y, RNGPos, speed, reset, vsync, en[6], gameOver, a7pixel);

  // Display the rocket and asteroids
  always_comb begin
   if (rpixel) 
			{r, g, b} = 24'hFFFFFF;
   else if (a1pixel|a2pixel|a3pixel|a4pixel|a5pixel|a6pixel|a7pixel) 
			{r, g, b} = 24'h8968CD;
   else
			{r, g, b} = gameOver? 24'hc61a09 : 24'h000000;
	end

endmodule

// display the rocket at the bottom middle of the screen
module rocket(input logic [9:0] x, y,
				input logic reset, vsync, keyright, keyleft, keyup, keydown, gameOver,
				output logic rpixel);
	
    // Data Structure for Rocket Shape
    logic [15:0][14:0] rocket_shape = {
	  15'b100000010000001,
      15'b110000010000011,
	  15'b111000111000111,
      15'b111110111011111,
	  15'b101111111111101,
      15'b100111010111001,
	  15'b000111000111000,
      15'b000111101111000,
      15'b000101111101000,
      15'b000000111000000,
	  15'b000000111000000,
      15'b000000111000000,
	  15'b000000111000000,
	  15'b000000111000000,
	  15'b000000010000000,
	  15'b000000010000000,
	  15'b000000010000000
    };

	logic [9:0] xleft, xright, ytop, ybottom, moveH, moveV;
	 
	 // Horizontal movement added to the rocket
	always_ff @(posedge vsync, posedge reset) begin
		if (reset)
			moveH <= 0;
		else if (gameOver)
				moveH <= moveH;
		else if ((keyright) && (moveH+10'd325 < 10'd633))
			moveH <= moveH + 10'd1;
		else if ((keyleft) && (moveH+10'd325 > 10'd18))
			moveH <= moveH - 10'd1;
		else
			moveH <= moveH;
	end
	
	// Vertical movement added to the rocket
	always_ff @(posedge vsync, posedge reset) begin
		if (reset)
			moveV <= 0;
		else if (gameOver)
			moveV <= moveV;
		else if ((keyup) && (moveV+10'd460 <= 10'd460)) 
			moveV <= moveV + 10'd1;
		else if ((keydown) && (moveV +10'd460 >= 10'd30))
			moveV <= moveV - 10'd1;
		else
			moveV <= moveV;
	end
	
	 assign xleft = 10'd312;
	 assign xright = 10'd326;
	 assign ytop = 10'd452;
	 assign ybottom = 10'd468;

  // Inside rocket module, assuming rocket starts at X=320, Y=460
    always_comb begin
        if ((x-moveH >= xleft) && (x-moveH <= xright) && 
            (y-moveV >= ytop) && (y-moveV < ybottom) && 
            (rocket_shape[y-ytop-moveV][x-xleft-moveH]))  begin
                rpixel = 1; // White
            end
        else begin
            rpixel = 0; // Black
        end
    end 
endmodule
	 
module asteroid(input logic [9:0] x, y, RNGPos, speed,
				input logic reset, vsync, en, gameOver,
                output logic apixel); 

// Data Structure for Asteroid Shape
logic [19:0][29:0] asteroid_shape = {
      30'b000000000000011100000000000000,
      30'b000000000000011100000000000000,
      30'b000000000011111111111000000000,
      30'b000000000011111111111000000000,
      30'b000000000011111111111111000000,
      30'b000000111111111111111111000000,
      30'b000000111111111111111111110000,
      30'b000000111111111111111111110000,
      30'b001111111111111111111111111000,
      30'b111111111111111111111111111111,
	  30'b111111111111111111111111111111,
      30'b001111111111111111111111111100,
      30'b111111111111111111111111111111,
      30'b000111111111111111111111111000,
      30'b000000011111111111111110000000,
      30'b000000011111111111111110000000,
	  30'b000000011111111111111110000000,
	  30'b000000000011111111111000000000,
	  30'b000000000011111111111000000000,
	  30'b000000000000000111111000000000
};
	
	 logic [9:0] xleft, xright, ytop, ybottom, moveDown, initialP;
	 
     // Asteroid moving down
	always_ff @(posedge vsync, posedge reset) begin
		if (reset) begin
			moveDown <= 0;
			initialP <= RNGPos;
		end
		else if (~en) begin
			moveDown <= 0;
			initialP <= RNGPos;
			end
		else if (gameOver)
			moveDown <= moveDown;
		else if (moveDown >= 10'd480) begin
			moveDown <= 0;
			initialP <= RNGPos;
		end
		else begin
			moveDown <= moveDown + speed;
			initialP <= initialP;
		end
	end

	 assign xleft = initialP;
	 assign xright = initialP + 10'd30;
	 assign ytop = 10'd0;
	 assign ybottom = 10'd20;


  // Inside asteroid module, assuming asteroid starts at X=310, Y=0
  always_comb begin
	if ((en) && (x >= xleft) && (x < xright) &&
        (y-moveDown+10'd470 >= ytop+10'd470) && (y-moveDown+10'd470 < ybottom+10'd470) &&
        (asteroid_shape[y-ytop-moveDown][x-xleft])) begin
	  apixel = 1; 
	end
	else begin
	  apixel = 0;
	end
  end
endmodule

module lfsr(input logic reset, clk,
				input logic [9:0] seed,
				output logic [9:0] RNG);		
	logic [9:0] count;
	
	always_ff @(posedge clk, posedge reset) begin
		if (reset)	begin		count <= seed;
		end
		else 					count <= {count[0] ^ count[9], count[9:1]};
	end 

	assign RNG = count % 10'd641;

endmodule

module asteroidEnable(input logic reset, clk,
						output logic [6:0] en);
	logic [6:0] count;

always_ff @(posedge clk, posedge reset) begin
	if (reset) begin
		count <= 7'b0000000;
	end
	else if (count == 7'b1111111) begin
		count <= count;
	end
	else begin
		count <= (count << 1) + 7'b0000001;
	end
	end

	assign en = count;

endmodule

module clock
	#(parameter width = 8)
	(input logic reset, vsync, enable,
				output logic clk);
	logic [width-1:0] count;

	always_ff @(posedge reset, posedge vsync) begin
		if (reset) begin
			count <= 0;
		end
		else if (~enable) begin
			count <= 0;
			end
		else begin
			count <= count + 1;
		end
	end

	assign clk = count[width-1];

endmodule

module asteroidSpeed (input logic reset, clk,
						output logic [9:0] speed);

	logic [9:0] count;

	always_ff @(posedge clk, posedge reset) begin
		if (reset) begin
			count <= 10'd1;
		end
		else if (count == 10'd4) begin
			count <= count;
		end
		else begin
			count <= count + 10'd1;
		end
	end

	assign speed = count;

endmodule

module gameState(input logic reset, clk, keyright, keyleft, keyup, keydown, 
							rpixel, a1pixel, a2pixel, a3pixel, a4pixel, a5pixel, a6pixel, a7pixel,
						output logic start, gameOver);
	
	typedef enum logic [1:0] {SR, S1, S2}
	statetype;
	statetype state, nextstate;
	
	always_ff @(posedge clk, posedge reset) begin
		if (reset) state <= SR;
		else       state <= nextstate;
	end
		
	always_comb begin
	case (state)
		SR: 	if (keyright | keyleft | keyup | keydown) nextstate = S1;
				else nextstate = SR;
		// Collison detection between rocket and asteroids
		S1:		if (rpixel & (a1pixel | a2pixel | a3pixel | a4pixel | a5pixel | a6pixel | a7pixel))	nextstate = S2;
				else nextstate = S1;
		S2:			nextstate = S2;
		default:	nextstate = SR;
	endcase
	end

assign start = (state == S1| state == S2);
assign gameOver = (state == S2);

endmodule


// Keep track of score using scoreclk
module score(input logic reset, scoreclk, gameOver,
				output logic [6:0] segments0,
				output logic [6:0] segments1,
				output logic [6:0] segments2,
				output logic [6:0] segments3);
	
	logic [6:0] count0;
	logic [6:0] count1;
	logic [6:0] count2;
	logic [6:0] count3;

	always_ff @(posedge scoreclk, posedge reset) begin
		if (reset) begin
			count0 <= 0;
			count1 <= 0;
			count2 <= 0;
			count3 <= 0;
			end
		else if (gameOver) begin
			count0 <= count0;
			count1 <= count1;
			count2 <= count2;
			count3 <= count3;
		end
		else if (count0 == 7'd9) begin
			count0 <= 0;
			if (count1 == 7'd9) begin
				count1 <= 0;
				if (count2 == 7'd9) begin
					count2 <= 0;
					if (count3 == 7'd9) begin
						count3 <= 0;
					end
					else begin
						count3 <= count3 + 1;
					end
				end
				else begin
					count2 <= count2 + 1;
				end
			end
			else begin
				count1 <= count1 + 1;
			end
		end
		else begin 
			count0 <= count0 + 1;
		end
		end

	sevenseg sevenseg0(count0, segments0);
	sevenseg sevenseg1(count1, segments1);
	sevenseg sevenseg2(count2, segments2);
	sevenseg sevenseg3(count3, segments3);


endmodule

// Display the score on the seven segment display
module sevenseg(input  logic [3:0] data,
                output logic [6:0] segments);

  always_comb
    case (data)
      //                     gfe_dcba
      0:       segments = 7'b100_0000;
      1:       segments = 7'b111_1001;
      2:       segments = 7'b010_0100;
      3:       segments = 7'b011_0000;
      4:       segments = 7'b001_1001;
      5:       segments = 7'b001_0010;
      6:       segments = 7'b000_0010;
      7:       segments = 7'b111_1000;
      8:       segments = 7'b000_0000;
      9:       segments = 7'b001_1000;
      10:      segments = 7'b000_1000;
      11:      segments = 7'b000_0011;
      12:      segments = 7'b010_0111;
      13:      segments = 7'b010_0001;
      14:      segments = 7'b000_0110;
      default: segments = 7'b000_1110;
    endcase
endmodule
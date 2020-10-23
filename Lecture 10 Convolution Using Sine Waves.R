#windows(7,10)
layout( matrix( c(1,2,3,4 ) , nrow=4 ,ncol=1 ,byrow=FALSE ) )
par(mar=c(3,3,1,0))
par(mgp=c(2,1,0))
par(mai=c(0.5,0.5,0.1,0.1))

nTheta = 1000

nCycles1 = 2.0
Theta1 = seq(from = 0, to = nCycles1*2.0*pi, by = nCycles1*2*pi/nTheta)
SinTheta1 = sin(Theta1)
plot( SinTheta1, type = "l", lwd = 3, main = "Sin f=2")

nCycles2 = 5.0
Theta2 = seq(from = 0, to = nCycles2*2.0*pi, by = nCycles2*2*pi/nTheta)
SinTheta2 = sin(Theta2)
plot( SinTheta2, type = "l", lwd = 3, main = "Sin f=5")

nCycles3 = 10.0
Theta3 = seq(from = 0, to = nCycles3*2.0*pi, by = nCycles3*2*pi/nTheta)
SinTheta3 = sin(Theta3)
plot( SinTheta3, type = "l", lwd = 3, main = "Sin f=10")

nCycles4 = 20.0
Theta4 = seq(from = 0, to = nCycles4*2.0*pi, by = nCycles4*2*pi/nTheta)
SinTheta4 = sin(Theta4)
plot( SinTheta4, type = "l", lwd = 3, main = "Sin f=20")

SinSum0 = SinTheta1 + SinTheta2 + SinTheta3 + SinTheta4 
plot( SinSum0, type = "l", lwd = 3, main = "Sin Sum w/o Signal")

nCyclesSignal = 50.0
ThetaSignal = seq(from = 0, to = nCyclesSignal*2.0*pi, by = nCyclesSignal*2*pi/nTheta)
SinThetaSignal = sin(ThetaSignal)
SinThetaSignal[1:400] = 0.0
SinThetaSignal[450:1000] = 0.0
plot( SinThetaSignal, type = "l", lwd = 3, main = "Sin Signal f=30")


SinSum = SinTheta1 + SinTheta2 + SinTheta3 + SinTheta4 + SinThetaSignal
plot( SinSum, type = "l", lwd = 3, main = "Sin Sum with Signal")

nCyclesMatch = 50.0
MatchTheta = seq(from = 0, to = nCyclesMatch*2.0*pi, by = nCyclesMatch*2*pi/nTheta)
SinMatch = sin(MatchTheta)
SinMatch[50:1000] = 0.0 
plot( MatchTheta, SinMatch, type = "l", lwd = 3, main = "Matched Filter")

Resp <- array(1000)
for (m in 1:1000) {
	Sum = 0	
	for (n in 1:20) {
		if ((n+m-10) < 1 ||(n+m-10) > 1000) Sum = 0 else Sum = Sum + SinSum[m+n-10]*SinMatch[n]
		Resp[m] = Sum^2
	}
}

layout( matrix( c(1,2,3,4 ) , nrow=1 ,ncol=1 ,byrow=FALSE ) )
plot( Resp, type = "l", lwd = 3, main = "Response")
plot( Resp, type = "l", lwd = 3, main = "Response")




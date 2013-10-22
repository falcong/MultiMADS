		MODULE TestObj_MOD
		USE REAL_PRECISION
		CONTAINS

		SUBROUTINE TestObj(X, OBJ)
		IMPLICIT NONE
		REAL(KIND = R8), DIMENSION(12), INTENT(IN):: X
		REAL(KIND = R8), DIMENSION(1), INTENT(OUT):: OBJ

		!local variables
		INTEGER::i, j, k, ii, m, N, iflag, cnt
		REAL(KIND = R8):: f1
		REAL(KIND = R8):: f2
		REAL(KIND = R8):: f3
		REAL(KIND = R8):: g
		REAL(KIND = R8)::SumXi, pi, zstar, dist, tol, s
		REAL(KIND = R8), DIMENSION(12):: lb, ub
		REAL(KIND = R8), DIMENSION(6,6):: Ip
		REAL(KIND = R8), DIMENSION(6):: beta, F, r

		N = 12
		m = 6
		pi = 3.14159265359
		SumXi = 0.0
		zstar=1.0
		tol=1.0e-13
		s = 0.0

		Ip(1:6,1:6) = 0
      forall (k = 1:6) Ip(k,k) = 1.0

      lb(1:12) = 0.0
		ub(1:12) = 1.0

		iflag=0
		do k=1,N
			if (X(k)<0.0.or.X(k)>1.0) then
				write(*,*)'infeasible ',X
				OBJ=1.0e13
				iflag=1
				exit
			end if
		end do
		
		if(iflag<1) then
			open(150, FILE="beta.dat", STATUS='OLD')
			read(150,*)beta
			close(150)
				r=MATMUL(Ip,beta)
				!	write(*,*)'beta and r ',beta,' ',r
				do j=6,N,1
					!SumXi = SumXi + (X(j)-0.5)**2 - Cos(20*pi*(X(j)-0.5))
					SumXi = SumXi + (X(j)-0.5)**2 
				end do
				!write(*,*) 'Sum ',SumXi
				!g   = 100 * (7.0 + SumXi)
				g   = SumXi
				!write(*,*) 'g ',g
				F(1) = abs((1+g)*Cos(X(1)*(pi/2))*Cos(X(2)*(pi/2))*Cos(X(3)*(pi/2))*Cos(X(4)*(pi/2))*Cos(X(5)*(pi/2)))
			  	F(2)  = abs((1+g)*Cos(X(1)*(pi/2))*Cos(X(2)*(pi/2))*Cos(X(3)*(pi/2))*Cos(X(4)*(pi/2))*Sin(X(5)*(pi/2)))
			  	F(3)  = abs((1+g)*Cos(X(1)*(pi/2))*Cos(X(2)*(pi/2))*Cos(X(3)*(pi/2))*Sin(X(4)*(pi/2)))
				F(4) = abs((1+g)*Cos(X(1)*(pi/2))*Cos(X(2)*(pi/2))*Sin(X(3)*(pi/2)))
				F(5) = abs((1+g)*Cos(X(1)*(pi/2))*Sin(X(2)*(pi/2)))
				F(6) = abs((1+g)*Sin(X(1)*(pi/2)))
				OBJ=0.0
				!	WRITE(*,*) 'Evaluate ', X, f1, f2, f3
				!	WRITE(*,*) 
				do i=1,m
					s = s + (r(i)-F(i))**2
				end do
				dist = SQRT(s)
				cnt = 1
				do i=1,m
					if (f(i)<r(i)) then
						cnt=cnt+1
					end if
				end do
				if(cnt==m) then
					OBJ = -(dist**2)
				else
					OBJ =	dist**2	
				end if

		OPEN(85, FILE="RSMInTest.dat", STATUS='OLD', POSITION='APPEND')
		WRITE(85,*)X,F,OBJ
		CLOSE(85)

		end if

		END SUBROUTINE TestObj

		END MODULE TestObj_MOD


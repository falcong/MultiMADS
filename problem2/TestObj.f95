		MODULE TestObj_MOD
		USE REAL_PRECISION
		CONTAINS

		SUBROUTINE TestObj(X, OBJ)
		IMPLICIT NONE
		REAL(KIND = R8), DIMENSION(2), INTENT(IN):: X
		REAL(KIND = R8), DIMENSION(1), INTENT(OUT):: OBJ

		!local variables
		INTEGER::i, j, k, ii, N, iflag
		REAL(KIND = R8):: f1
		REAL(KIND = R8):: f2
		REAL(KIND = R8):: f3
		REAL(KIND = R8):: g, h
		REAL(KIND = R8)::SumXi, pi, zstar, dist, tol
		REAL(KIND = R8), DIMENSION(2):: lb, ub
		REAL(KIND = R8), DIMENSION(3,3):: Ip
		REAL(KIND = R8), DIMENSION(3):: beta, F, r
		REAL(KIND = R8), DIMENSION(5):: delta

		N = 2
		pi = 3.14159265359
		SumXi = 0.0
		zstar=1
		tol=1.0e-13
		data delta/0,0.25,0.50,0.75,1/

		Ip(1:3,1:3) = 0
      forall (k = 1:3) Ip(k,k) = 1

      lb(1:2) = 0.0
		ub(1:2) = 1.0

		f1=0
		f2=0
		f3=0
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
				g   = 1 + 10*X(2)
				!write(*,*) 'g ',g
				h = 1 - (X(1)/g)**2 - (X(1)/g)*Sin(8*pi*X(2))

				f1 = X(1)
			  	f2  = g*h
				if(X(1)<=0.5) then
				  	f3  = 0
				else
					f3=1
				end if
				F(1)=f1
				F(2)=f2
				F(3)=f3
			!	WRITE(*,*) 'Evaluate ', X, f1, f2, f3
			!	WRITE(*,*) 
				dist = SQRT( ( (r(1)-F(1))**2 )+( (r(2)-F(2))**2 )+( (r(3)-F(3))**2 ))
				if (f1<r(1).and.f2<r(2).and.f3<r(3)) then
					OBJ = -(dist**2)
				else
					OBJ =	dist**2	
				end if

		OPEN(85, FILE="RSMInTest.dat", STATUS='OLD', POSITION='APPEND')
		WRITE(85,*)X,f1,f2,f3
		CLOSE(85)

		end if

		END SUBROUTINE TestObj

		END MODULE TestObj_MOD


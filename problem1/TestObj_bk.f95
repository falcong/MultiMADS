		MODULE TestObj_MOD
		USE REAL_PRECISION
		CONTAINS

		SUBROUTINE TestObj(X, OBJ)
		IMPLICIT NONE
		REAL(KIND = R8), DIMENSION(12), INTENT(IN):: X
		REAL(KIND = R8), DIMENSION(1), INTENT(OUT):: OBJ

		!local variables
		INTEGER::i, j, k, ii, N, iflag
		REAL(KIND = R8):: f1
		REAL(KIND = R8):: f2
		REAL(KIND = R8):: f3
		REAL(KIND = R8):: g
		REAL(KIND = R8)::SumXi, pi, zstar, dist, tol
		REAL(KIND = R8), DIMENSION(12):: lb, ub
		REAL(KIND = R8), DIMENSION(3,3):: Ip
		REAL(KIND = R8), DIMENSION(3):: beta, F, r
		REAL(KIND = R8), DIMENSION(5):: delta

		N = 12
		pi = 3.14159265359
		SumXi = 0.0
		zstar=0.5
		tol=1.0e-13
		data delta/0,0.25,0.50,0.75,1/

		Ip(1:3,1:3) = 0
      forall (k = 1:3) Ip(k,k) = 0.5

      lb(1:12) = 0.0
		ub(1:12) = 1.0

		f1=0
		f2=0
		f3=0
		iflag=0
		do k=1,N
			if (X(k)<0.0.or.X(k)>1.0) then
!				write(*,*)'infeasible ',X
				OBJ=1.0e13
				iflag=1
				exit
			end if
		end do
		
		if(iflag<1) then
			open(150, FILE="beta.dat", STATUS='OLD')
			read(150,*)beta
			close(150)
		!	do i=1,1,1
		!		do j=1,1,1
			!	beta(1)=0.5
			!	beta(2)=1
			!	beta(3)=1-(beta(1)+beta(2))
			!	write(*,*)'beta ',beta
				r=MATMUL(Ip,beta)
			!	write(*,*)'beta and r ',beta,' ',r
				do ii=3,N,1
					SumXi = SumXi + ((X(ii)-0.5)**2 - Cos(20*pi*(X(ii)-0.5)))
				end do
				!write(*,*) 'Sum ',SumXi
				g   = 100 * (10.0 + SumXi);
				!write(*,*) 'g ',g
				f1 = 0.5*X(1)*X(2)*(1+g)
			  	f2  = 0.5*X(1)*(1-X(2))*(1+g)
			  	f3  = 0.5*(1-X(1))*(1+g)
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
	!		end do
	!	end do


		OPEN(85, FILE="RSMInTest.dat", STATUS='OLD', POSITION='APPEND')
		WRITE(85,*)X,f1,f2,f3, OBJ
		CLOSE(85)

		end if

		END SUBROUTINE TestObj

		END MODULE TestObj_MOD


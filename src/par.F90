!------------------------------------------------------------------------------
! MODULE: seq
!
!> @author
!> osdnk
!
! DESCRIPTION: 
!> Algorithms implementations.
!
!------------------------------------------------------------------------------

#define USE_DOT 0
#define USE_CACHE 1

module seq
  contains
  !------------------------------------------------------------------------------
  !> @author
  !> osdnk
  !
  ! DESCRIPTION: 
  !> Multiplies two matrices  
  !
  !> @param[in] first First matrix
  !> @param[in] second Second matrix
  !> @param[out] multiply result
  !> @param[out] status code, 0 is success
  !------------------------------------------------------------------------------
    subroutine mm(first, second, multiply, ret)

      implicit none
      real (kind = 8), intent(in) :: first(:, :)
      real (kind = 8), intent(in) :: second(:, :)
      real (kind = 8), intent(out) :: multiply(:, :)
      integer(kind = 4) :: fxy, fx, fy, sxy, sx, sy, mx, my

      integer (kind = 4), intent(out) :: ret
      integer(kind = 4) :: i, j, k, ii, jj, ichunk
      real (kind = 8) :: sum

      ichunk = 1024

      #if USE_DOT
         write(*,*) "Using dot!"
      #else
         write(*,*) "Not using dot!"
      #endif
#if USE_CACHE
         write(*,*) "Using cache!"
      #else
         write(*,*) "Not using cache!"
      #endif

      fx = SIZE(first(1, :)) !number of colums in first matrix
      fy = SIZE(first(:, 1)) !number of rows in first matrix
      fxy = SIZE(first) !number of rows and columns in first matrix
      sx = SIZE(second(1, :)) !number of colums in second matrix
      sy = SIZE(second(:, 1)) !number of rows in second matrix
      sxy = SIZE(second) !number of rows and columns in second matrix
      mx = SIZE(multiply(1, :))
      my = SIZE(multiply(:, 1))

      IF (fx == sy .AND. fx * fy == fxy .AND. sx * sy == sxy .AND. fy == my .AND. sx == mx) THEN
        sum = 0.d0
        multiply = 0.d0
        #if USE_CACHE
            do i = 1, my! columns in mmultiply
          do j = 1, mx ! rows in multiply
            #if USE_DOT
                    multiply(i,j)=dot_product(first(i,:),second(:,j))
          #else
                    do k = 1, fx
            sum = sum + first(i, k) * second(k, j)
          end do
          multiply(i, j) = sum
          sum = 0.d0
            #endif

          end do
        end do
        #else
            do ii = 1, my, ichunk
          do jj = 1, mx, ichunk
            do i = ii, min(ii + ichunk - 1, my)! columns in mmultiply
              do j = jj, min(jj + ichunk - 1, mx) ! rows in multiply
                #if USE_DOT
                             multiply(i,j)=dot_product(first(i,:),second(:,j))
              #else
                            do k = 1, fx
                sum = sum + first(i, k) * second(k, j)
              end do
              multiply(i, j) = sum
              sum = 0.d0
                #endif
                        end do
            end do
          end do
        end do
        #endif
            ret = 0.d0
      ELSE
        ret = 1.d0
      END IF
    end subroutine mm


  !------------------------------------------------------------------------------
  !> @author
  !> osdnk
  !
  ! DESCRIPTION: 
  !> Performs gaussian elimination
  !
  !> @param[inout] A Rank 2 array of coefficients
  !> @param[inout] X Rank 1 array of values.
  !> @param[in] n Number of rows
  !------------------------------------------------------------------------------
  subroutine gauss_eliminate(A, X, n)
    integer(kind=8), intent(in) :: n
    real(kind = 8), intent(inout) :: A(0:N, 0:N), X(0:N)
    real ( kind = 8), codimension[:], allocatable :: concurrA(:,:)
    real ( kind = 8), codimension[:], allocatable :: concurrX(:)
    real(kind = 8) :: r !ratio
    integer(kind=8) :: i, j

    allocate(concurrA(0:N, 0:N)[])
    allocate(concurrX(0:N)[])

    if (THIS_IMAGE() .eq. 1) then
      concurrA(:,:)[1] = A(:,:)
      concurrX(:)[1] = X(:)
    end if

    do i = 0, N
      ! scale row i to have 1 on the diagonal
      if(THIS_IMAGE() .eq. 1) then
        concurrX(i)[1] = concurrX(i)[1] / concurrA(i, i)[1]
        concurrA(:, i)[1] = concurrA(:, i)[1] / concurrA(i, i)[1]
      end if
      sync all
      do j = THIS_IMAGE() - 1, N, NUM_IMAGES()
        IF ((i .NE. j) .AND. (ABS(concurrA(i, i)[1] - 0) > 1d-6)) THEN
          r = concurrA(i, j)[1] / concurrA(i, i)[1]
          concurrA(:,j)[1] = concurrA(:,j)[1] - r * concurrA(:, i)[1]
          concurrX(j)[1] = concurrX(j)[1] - r * concurrX(i)[1]
        END IF
      END DO
    END DO


    if (THIS_IMAGE() .eq. 1) then
      A(:,:) = concurrA(:,:)[1]
      X(:) = concurrX(:)[1]
    end if

    deallocate(concurrA)
    deallocate(concurrX)
  end subroutine gauss_eliminate
end module
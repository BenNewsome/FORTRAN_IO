
      MODULE Subroutines
      CONTAINS
      SUBROUTINE Calculate_array_1(Array,Size1)
      implicit none
      integer::i,j
      integer, intent(in):: Size1
      double precision, dimension(:,:), intent(inout) :: Array
      double precision  :: random
!$OMP DO
!$OMP+PRIVATE( i, j, random )


      
      do j=1,Size1
         do i=1,Size1

            CALL cpu_time(random)
            random=LOG(random)
            random=sin(random**0.002748392567)
            Array(i,j) = random

         enddo
      enddo
!$OMP END DO
      END SUBROUTINE !Calculate_array_1




      SUBROUTINE Calculate_array_2(Array,Size1)
      implicit none
      integer::i,j
      integer, intent(in):: Size1
      double precision, dimension(:,:), intent(inout) :: Array
      double precision  :: random
!$OMP DO
!$OMP+PRIVATE( i, j, random )


      
      do j=1,Size1
         do i=1,Size1

            CALL cpu_time(random)
            random=LOG(random)
            random=sin(random**0.002748392567)
            Array(i,j) = random

         enddo
      enddo
!$OMP END DO
      END SUBROUTINE !Calculate_array_1



      SUBROUTINE Write_Array_1(Array, Size1)
      implicit none
      integer::i,j
      integer, intent(in)::Size1
      double precision, dimension(:,:), intent(in) :: Array


!$OMP SINGLE 
!$OMP+PRIVATE(i,j)
   
      
      do j=1,Size1
         do i=1,Size1
            write(11,*)i,j,Array(i,j)
         end do
      end do
      print*,'Write done'

!$OMP END SINGLE NOWAIT


      END SUBROUTINE !Wrire_Array_1


      END MODULE !Subroutines

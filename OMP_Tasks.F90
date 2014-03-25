
      Program Write_test
      USE omp_lib
      USE Subroutines
      IMPLICIT NONE
      
      INTEGER     :: i, j  ! Loop counters
      INTEGER     :: Size1, Size2 !Array Sizes
      INTEGER     :: Number_of_threads

      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE  :: Array_1, Array_2
      DOUBLE PRECISION  :: random
      DOUBLE PRECISION  :: start_loop_1, start_loop_2 
      DOUBLE PRECISION  :: end_loop_1, end_loop_2 
      Size1 = 3000
      Size2 = 5000

      Allocate(Array_1(Size1,Size1))
      Allocate(Array_2(Size2,Size2))

      Open(unit=11, file="Array1.dat")




      CALL cpu_time(start_Loop_1)

      print*,'Starting'

! Do maths on array 1


!$OMP PARALLEL
      Call Calculate_Array_1(Array_1, Size1)
!$OMP END PARALLEL
      
!!$OMP PARALLEL DO
!!$OMP+PRIVATE( i, j, random )


      
!      do j=1,Size1
!         do i=1,Size1
!
!            CALL cpu_time(random)
!            random=LOG(random)
!            random=sin(random**0.002748392567)
!            Array_1(i,j) = random
!
!         enddo
!      enddo
!!$OMP END PARALLEL DO



      print*,'Array 1 made'
      CALL cpu_time(end_Loop_1)


! Write array 1
!$OMP PARALLEL

!OMP TAKS
      Call Write_Array_1(Array_1, Size1)
!OMP END TASK
 
!!$OMP SINGLE 
!!$OMP+PRIVATE(i,j)
!   
!      
!      do j=1,Size1
!         do i=1,Size1
!            write(11,*)i,j,Array_1(i,j)
!         end do
!      end do
!      print*,'Write done'

!!$OMP END SINGLE NOWAIT

      
      CALL cpu_time(start_Loop_2)


!$OMP TASK
      Call Calculate_array_2(Array_2, Size2)
!$OMP END TASK


!! Do maths on array 2
!!$OMP TASK
!!$OMP DO
!!$OMP+PRIVATE( i, j, random )


!      do j=1,Size2
!         do i=1,Size2
!
!            CALL cpu_time(random)
!            random=LOG(random)
!            random=sin(random**0.002748392567)
!            Array_2(i,j) = random
!
!         enddo
!      enddo


!!$OMP END DO


!!$OMP END TASK


      Number_of_Threads = omp_get_num_threads()

!$OMP END PARALLEL
      print*,'Array 2 done'
      CALL cpu_time(end_Loop_2)
!Print results


      print*,' Results from omp_tasks run'
      print*,' Number of threads = ',Number_of_Threads
      print*,'Total time = ', abs(Start_loop_1 - End_loop_2)
      print*,'Loop 1 time = ', abs(Start_loop_1 - End_loop_1)
      print*,'Loop 2 time = ', abs(Start_loop_2 - End_loop_2)
      print*,'Write time = ', abs(End_loop_1 - Start_loop_2)


      DEALLOCATE(Array_1, Array_2)

      END PROGRAM

!end



!>@file  t_work_time.f90
!!       module t_work_time
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in 2001
!
!> @brief routines to count elapsed time
!!
!!@verbatim
!!      subroutine alloc_elapsed_timer(elps)
!!      subroutine append_elapsed_timer                                 &
!!     &         (num_append, iend_org, iend_new, elps)
!!      subroutine dealloc_elapsed_timer(elps)
!!
!!      subroutine start_elapsed_timer(iflag_elps, elps)
!!      subroutine end_elapsed_timer(iflag_elps, elps)
!!      subroutine reset_elapsed_timer(istart, iend, elps)
!!
!!      subroutine output_elapsed_log                                   &
!!     &         (iflag_time_4_each_pe, time_file_prefix, elps)
!!        type(elapsed_time_data), intent(inout) :: elps
!!@endverbatim
!!
!!@params  timer ID
!
      module t_work_time
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      integer(kind = kint), parameter, private :: id_timer_file = 13
!
!>      Structure of elapsed time data display
      type elapsed_time_data
!>        Number of elapsed log items
        integer(kind = kint) :: num_elapsed
!>        Elapsed time label
        character (len=kchara), allocatable :: labels(:)
!
!>        Start time of wall clock time
        real (kind=kreal), allocatable :: start_time(:)
!>        Current time of wall clock time
        real (kind=kreal), allocatable :: elapsed(:)
!
!>        Average of wall clock time
        real(kind=kreal), allocatable :: elapsed_ave(:)
!>        Standard deviation of wall clock time
        real(kind=kreal), allocatable :: elapsed_std(:)
!>        Minimum of wall clock time
        real(kind=kreal), allocatable :: elapsed_min(:)
!>        Maximum of wall clock time
        real(kind=kreal), allocatable :: elapsed_max(:)
!>        Square of deviation of wall clock time from average
        real(kind=kreal), allocatable :: elapsed_sqr(:)
      end type elapsed_time_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_elapsed_timer(elps)
!
      type(elapsed_time_data), intent(inout) :: elps
!
!
      allocate(elps%elapsed(elps%num_elapsed))
      allocate(elps%elapsed_ave(elps%num_elapsed))
      allocate(elps%elapsed_std(elps%num_elapsed))
      allocate(elps%elapsed_sqr(elps%num_elapsed))
      allocate(elps%elapsed_min(elps%num_elapsed))
      allocate(elps%elapsed_max(elps%num_elapsed))
      allocate(elps%start_time(elps%num_elapsed))
      allocate(elps%labels(elps%num_elapsed))
!
      if(elps%num_elapsed .le. 0) return
        elps%start_time =  zero
        elps%elapsed =     zero
        elps%elapsed_ave = zero
        elps%elapsed_std = zero
        elps%elapsed_sqr = zero
        elps%elapsed_min = zero
        elps%elapsed_max = zero
!
      end subroutine alloc_elapsed_timer
!
! ----------------------------------------------------------------------
!
      subroutine append_elapsed_timer                                   &
     &         (num_append, iend_org, iend_new, elps)
!
      integer(kind = kint), intent(in) :: num_append
!
      integer(kind = kint), intent(inout) :: iend_org, iend_new
      type(elapsed_time_data), intent(inout) :: elps
!
      character(len=kchara), allocatable :: tmp_label(:)
!
!
      iend_org = elps%num_elapsed
      iend_new = elps%num_elapsed + num_append
!
      allocate(tmp_label(iend_org))
      tmp_label(1:iend_org) = elps%labels(1:iend_org)
!
      call dealloc_elapsed_timer(elps)
!
      elps%num_elapsed = iend_new
      call alloc_elapsed_timer(elps)
      elps%labels(1:iend_org) = tmp_label(1:iend_org)
      deallocate(tmp_label)
!
      end subroutine append_elapsed_timer
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_elapsed_timer(elps)
!
      type(elapsed_time_data), intent(inout) :: elps
!
!
      deallocate(elps%elapsed, elps%elapsed_ave)
      deallocate(elps%elapsed_std, elps%elapsed_sqr)
      deallocate(elps%elapsed_min, elps%elapsed_max)
      deallocate(elps%start_time)
      deallocate(elps%labels)
!
      end subroutine dealloc_elapsed_timer
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine start_elapsed_timer(iflag_elps, elps)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: iflag_elps
      type(elapsed_time_data), intent(inout) :: elps
!
!
      elps%start_time(iflag_elps) = MPI_WTIME()
!      if(iflag_elps .eq. 3) call summary_start()
!      call hpm_start(elps%labels(iflag_elps))
!
      end subroutine start_elapsed_timer
!
! ----------------------------------------------------------------------
!
      subroutine end_elapsed_timer(iflag_elps, elps)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: iflag_elps
      type(elapsed_time_data), intent(inout) :: elps
!
!
!      if(iflag_elps .eq. 3) call summary_stop()
!      call hpm_stop(elps%labels(iflag_elps)) 
      elps%elapsed(iflag_elps) = elps%elapsed(iflag_elps)               &
     &                     + MPI_WTIME() - elps%start_time(iflag_elps)
!
      end subroutine end_elapsed_timer
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapsed_timer(istart, iend, elps)
!
      integer(kind = kint), intent(in) :: istart, iend
      type(elapsed_time_data), intent(inout) :: elps
!
!
      elps%elapsed(istart:iend) = zero
!
      end subroutine reset_elapsed_timer
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_elapsed_log                                     &
     &         (iflag_time_4_each_pe, time_file_prefix, elps)
!
      use calypso_mpi
      use calypso_mpi_real
      use set_parallel_file_name
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: iflag_time_4_each_pe
      character(len=kchara), intent(in) :: time_file_prefix
!
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint) :: i
      character(len=kchara) :: fname_tmp, file_name
!
!
      call calypso_mpi_allreduce_real(elps%elapsed, elps%elapsed_ave,   &
     &    cast_long(elps%num_elapsed), MPI_SUM)
      call calypso_mpi_reduce_real(elps%elapsed, elps%elapsed_min,      &
     &    cast_long(elps%num_elapsed), MPI_MIN, 0)
      call calypso_mpi_reduce_real(elps%elapsed, elps%elapsed_max,      &
     &    cast_long(elps%num_elapsed), MPI_MAX, 0)
!
!$omp parallel workshare
      elps%elapsed_ave(1:elps%num_elapsed)                              &
     &     = elps%elapsed_ave(1:elps%num_elapsed) / dble(nprocs)
      elps%elapsed_sqr(1:elps%num_elapsed)                              &
     &     = (elps%elapsed(1:elps%num_elapsed)                          &
     &      - elps%elapsed_ave(1:elps%num_elapsed))**2
!$omp end parallel workshare
!
      call calypso_mpi_reduce_real(elps%elapsed_sqr, elps%elapsed_std,  &
     &    cast_long(elps%num_elapsed), MPI_MAX, 0)
!
      if(iflag_time_4_each_pe .gt. 0) then
        fname_tmp = add_process_id(my_rank, time_file_prefix)
        file_name = add_dat_extension(fname_tmp)
        open(id_timer_file,file=file_name,position='append')
        write(id_timer_file,*) 'Average elapsed time'
        do i = 1, elps%num_elapsed
          if(elps%elapsed(i) .gt. zero) then
            write(id_timer_file,'(i3,a2,a,a2,1pe20.11)') i, '. ',       &
     &         trim(elps%labels(i)), ': ', elps%elapsed(i)
          end if
        end do
      end if
!
!
      if (my_rank .ne. 0) return
!
!$omp parallel workshare
      elps%elapsed_std(1:elps%num_elapsed)                              &
     &     = sqrt(elps%elapsed_std(1:elps%num_elapsed) / dble(nprocs))
!$omp end parallel workshare
!
      file_name = add_dat_extension(time_file_prefix)
      open(id_timer_file,file=file_name,position='append')
      write(id_timer_file,'(a)') 'Elapsed time'
      write(id_timer_file,'(a)') 'Average and standard deviation'
      do i = 1, elps%num_elapsed
        if(elps%elapsed_ave(i) .gt. zero) then
          write(id_timer_file,'(i3,a2,1p2e15.6,a2,a)')                  &
     &        i, ': ', elps%elapsed_ave(i), elps%elapsed_std(i),        &
     &           ': ', trim(elps%labels(i))
        end if
      end do
!
      write(id_timer_file,'(a)') ''
      write(id_timer_file,'(a)') 'Minimum and maximum elapsed time'
      do i = 1, elps%num_elapsed
        if(elps%elapsed_ave(i) .gt. zero) then
          write(id_timer_file,'(i3,a2,1p2e15.6,a2,a)')                  &
     &            i, ': ', elps%elapsed_min(i), elps%elapsed_max(i),    &
     &               ': ', trim(elps%labels(i))
         end if
      end do
      close(id_timer_file)
!
      end subroutine output_elapsed_log
!
! ----------------------------------------------------------------------
!
      end module t_work_time

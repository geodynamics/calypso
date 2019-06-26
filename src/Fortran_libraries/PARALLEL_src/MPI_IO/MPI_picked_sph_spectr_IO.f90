!>@file   MPI_picked_sph_spectr_IO.f90
!!@brief  module MPI_picked_sph_spectr_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_picked_spectrum_file                          &
!!     &         (time_d, sph_rj, rj_fld, picked)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
!!
!!      subroutine write_picked_specr_head_mpi(IO_param, picked)
!!        type(picked_spectrum_data), intent(in) :: picked
!!
!!      function picked_each_mode_to_text                               &
!!     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, d_rj_out)
!!@endverbatim
!!
      module MPI_picked_sph_spectr_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
      use t_calypso_mpi_IO_param
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_data
      use t_time_data
!
      implicit  none
!
      type(calypso_MPI_IO_params), save, private :: IO_param1
!
      integer, parameter, private :: len_fixed = 4*16 + 2*25 + 1
!
      private :: write_picked_specr_data_mpi
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_picked_spectrum_file                            &
     &         (time_d, sph_rj, rj_fld, picked)
!
      use MPI_ascii_data_IO
      use set_parallel_file_name
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      character(len = kchara) :: file_name
!
!
!
      if(picked%num_sph_mode .le. 0) return
!
      file_name = add_dat_extension(picked%file_prefix)
!
!      if(my_rank .eq. 0) then
!        call write_picked_spectr_header_only(file_name, picked)
!      end if
!      call calypso_mpi_barrier
!
      call open_append_mpi_file(file_name, nprocs, my_rank, IO_param1)
      call calypso_mpi_barrier
!
      if(IO_param1%ioff_gl .eq. 0) then
        call write_picked_specr_head_mpi(IO_param1, picked)
      end if
!
      call write_picked_specr_data_mpi(IO_param1, time_d, sph_rj,       &
     &    rj_fld, picked, picked%ntot_comp_rj)
!
      call close_mpi_file(IO_param1)
!
      end subroutine append_picked_spectrum_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_picked_specr_head_mpi(IO_param, picked)
!
      use MPI_ascii_data_IO
      use write_field_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: i
      integer :: len_head, len_fld, len_each
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = 1), parameter :: timebuf = char(10)
      character(len = kchara) :: textbuf
!
!
      len_head = len(pick_sph_header_no_field(picked))
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_head, pick_sph_header_no_field(picked))
!
        len_fld = 0
        do i = 1, picked%ntot_comp_rj
          len_each = len_trim(picked%spectr_name(i)) + 4
          len_fld = len_fld + len_each
          write(textbuf,'(a,a4)') trim(picked%spectr_name(i)), '    '
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, len_each, textbuf)
        end do
!
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, 1, timebuf)
      end if
!
      call MPI_BCAST(len_fld, 1, CALYPSO_GLOBAL_INT, 0,                 &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + len_head + len_fld + ione
!
      end subroutine write_picked_specr_head_mpi
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_picked_specr_data_mpi(IO_param, time_d,          &
     &          sph_rj, rj_fld, picked, ntot_comp_rj)
!
      use pickup_sph_spectr_data
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
!
      integer(kind = kint) :: inum, knum, ipick, ist
!
      integer(kind = kint_gl) :: num
      integer :: ilen_n
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = len_fixed+ntot_comp_rj*25),                       &
     &                                 allocatable :: pickedbuf(:)
      real(kind=kreal), allocatable :: d_rj_out(:)
!
!
      ilen_n = len_fixed + ntot_comp_rj*25
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &         + ilen_n * picked%istack_picked_spec_lc(my_rank)
!
        allocate(d_rj_out(ntot_comp_rj))
        allocate(pickedbuf(num))
        ist = 0
        if(picked%idx_out(0,4) .gt. 0) then
          ist = 1
          call pick_degre0_sped_4_monitor                               &
     &       (rj_fld, picked, ntot_comp_rj, d_rj_out)
          pickedbuf(1)                                                  &
     &           = picked_each_mode_to_text                             &
     &           (time_d%i_time_step, time_d%time,                      &
     &            zero, izero, izero, izero, ntot_comp_rj, d_rj_out)
        end if
!
        do inum = 1, picked%num_sph_mode_lc
          do knum = 1, picked%num_layer
             ipick = knum + (inum-1) * picked%num_layer
             call pick_single_sph_spec_4_monitor(inum, knum,            &
     &           sph_rj, rj_fld, picked, ntot_comp_rj, d_rj_out)
!
             pickedbuf(ipick+ist)                                       &
     &         = picked_each_mode_to_text                               &
     &           (time_d%i_time_step, time_d%time,                      &
     &            picked%radius_gl(knum), picked%id_radius(knum),       &
     &            picked%idx_out(inum,1), picked%idx_out(inum,2),       &
     &            ntot_comp_rj, d_rj_out)
          end do
        end do
!
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_file, ioffset, ilen_n, num, pickedbuf)
        deallocate(d_rj_out, pickedbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &       + ilen_n * picked%istack_picked_spec_lc(nprocs)
!
      end subroutine write_picked_specr_data_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function picked_each_mode_to_text                                 &
     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, d_rj_out)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time, radius
      integer(kind = kint), intent(in) :: kr, l, m
      integer(kind = kint), intent(in) :: ntot_comp_rj
      real(kind = kreal), intent(in) :: d_rj_out(ntot_comp_rj)
!
      character(len = len_fixed+ntot_comp_rj*25)                        &
     &                          :: picked_each_mode_to_text
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a37,i4,a17)')                                     &
     &         '(i16,1pe25.14e3, i16,1pe25.14e3,2i16,',                 &
     &           ntot_comp_rj, '(1pE25.14e3), a1)'
      write(picked_each_mode_to_text,fmt_txt) i_step, time,             &
     &          kr, radius, l, m, d_rj_out(1:ntot_comp_rj), char(10)
!
      end function  picked_each_mode_to_text
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_picked_spectr_header_only(file_name, picked)
!
      use write_field_labels
!
      character(len=kchara), intent(in) :: file_name
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: i
      integer(kind = kint), parameter :: id_pick_mode = 23
!
!
      open(id_pick_mode, file = file_name, form='formatted',            &
     &    status='old', position='append', err = 99)
      close(id_pick_mode)
      return
!
   99 continue
      close(id_pick_mode)
!
      open(id_pick_mode, file = file_name, form='formatted',            &
     &    status='replace')
      call write_pick_sph_file_header(id_pick_mode, picked)
      close(id_pick_mode)
!
      end subroutine write_picked_spectr_header_only
!
! ----------------------------------------------------------------------
!
      end module MPI_picked_sph_spectr_IO

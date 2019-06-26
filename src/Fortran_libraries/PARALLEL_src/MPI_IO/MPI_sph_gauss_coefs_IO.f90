!>@file   MPI_sph_gauss_coefs_IO.f90
!!@brief  module MPI_sph_gauss_coefs_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_sph_gauss_coefs_file                          &
!!     &         (time_d, sph_params, sph_rj, ipol, rj_fld, gauss)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: gauss
!!        type(calypso_MPI_IO_params) :: IO_param
!!@endverbatim
!!
      module MPI_sph_gauss_coefs_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
      use t_calypso_mpi_IO_param
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_address
      use t_phys_data
      use t_time_data
      use m_monitor_file_labels
!
      implicit  none
!
      integer(kind = kint), parameter, private :: id_gauss_coef = 23
!
      private :: write_sph_gauss_coefes_mpi
      private :: picked_gauss_head
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_sph_gauss_coefs_file                            &
     &         (time_d, sph_params, sph_rj, ipol, rj_fld, gauss)
!
      use MPI_ascii_data_IO
      use set_parallel_file_name
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
!
      type(calypso_MPI_IO_params) :: IO_param
      character(len = kchara) :: file_name
!
!
      if(gauss%num_sph_mode .le. 0) return
!
      file_name = add_dat_extension(gauss%file_prefix)
      call open_append_mpi_file(file_name, nprocs, my_rank, IO_param)
      call calypso_mpi_barrier
!
      if(IO_param%ioff_gl .eq. 0) then
        call write_sph_gauss_header_mpi(IO_param, gauss)
      end if
!
      call write_sph_gauss_coefes_mpi                                   &
     &   (IO_param, time_d, sph_params, sph_rj, ipol, rj_fld, gauss)
!
      call close_mpi_file(IO_param)
!
      end subroutine append_sph_gauss_coefs_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_sph_gauss_header_mpi(IO_param, picked)
!
      use MPI_ascii_data_IO
      use write_field_labels
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: len_fld, i
      integer :: len_each
      integer, parameter :: ilen_n = 14
      integer, parameter :: ilen_h                                      &
     &        = ilen_pk_gauss_head + 16+25+1 + ilen_time_label
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = ilen_h) :: timebuf
      character(len = kchara) :: textbuf
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        write(timebuf,'(a, i16,1pe25.15e3,a1, a)')                      &
     &        hd_pick_gauss_head(),                                     &
     &        picked%num_sph_mode, picked%radius_gl(1), char(10),       &
     &        hd_time_label()
!
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ilen_h, timebuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_h
!
!
      len_fld = count_label_list_length(picked%num_sph_mode_lc,         &
     &                                  picked%gauss_mode_name_lc)
      call istack64_4_parallel_data(cast_long(len_fld), IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(my_rank)
      if(len_fld .gt. 0) then
        do i = 1, picked%num_sph_mode_lc
          len_each = len_trim(picked%gauss_mode_name_lc(i)) + 4
          write(textbuf,'(a,a4)') trim(picked%gauss_mode_name_lc(i)),   &
     &                           '    '
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, len_each, textbuf)
        end do
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(nprocs)
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, 1, char(10))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ione
!
!
      end subroutine write_sph_gauss_header_mpi
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_sph_gauss_coefes_mpi(IO_param, time_d,           &
     &          sph_params, sph_rj, ipol, rj_fld,  picked)
!
      use MPI_ascii_data_IO
      use pickup_gauss_coefficients
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
!
      integer(kind = kint) :: inum
!
      integer(kind = kint_gl) :: num
      integer :: ilength
      integer, parameter :: ilen_n = 25
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = 16+25) :: timebuf
      character(len = 25), allocatable :: pickedbuf(:)
      real(kind=kreal), allocatable :: d_rj_out(:)
!
!
!
      ilength = len(picked_gauss_head(time_d%i_time_step, time_d%time))
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        timebuf = picked_gauss_head(time_d%i_time_step, time_d%time)
!
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ilength, timebuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ilength
!
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &         + ilen_n * picked%istack_picked_spec_lc(my_rank)
!
        allocate(d_rj_out(picked%num_sph_mode_lc))
        allocate(pickedbuf(picked%num_sph_mode_lc))
!
        call gauss_coefficients_4_write                                 &
     &     (sph_params, sph_rj, ipol, rj_fld, picked, d_rj_out)
        do inum = 1, picked%num_sph_mode_lc
          write(pickedbuf(inum),'(1pE25.14e3)') d_rj_out(inum)
        end do
!
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_file, ioffset, ilen_n, num, pickedbuf)
        deallocate(d_rj_out, pickedbuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                + ilen_n * picked%istack_picked_spec_lc(nprocs)
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        write(timebuf,'(a1)') char(10)
!
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, 1, timebuf)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ione
!
!
      end subroutine write_sph_gauss_coefes_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      character(len = 16+25) function picked_gauss_head(i_step, time)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
!
      write(picked_gauss_head,'(i16,1pe25.14e3)') i_step, time
!
      end function  picked_gauss_head
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
     integer(kind = kint) function check_gauss_coefs_num(gauss)
!
      use set_parallel_file_name
!
      type(picked_spectrum_data), intent(in) :: gauss
!!
      character(len = kchara) :: file_name
!
!
      check_gauss_coefs_num = 0
      if(gauss%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      file_name = add_dat_extension(gauss%file_prefix)
      open(id_gauss_coef, file = file_name,                             &
     &    form='formatted', status='old', err = 99)
!
      check_gauss_coefs_num = check_gauss_coefs_4_monitor(gauss)
      close(id_gauss_coef)
      return
!
  99  continue
      write(*,*) 'No Gauss coefficient file'
      return
!
      end function check_gauss_coefs_num
!
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &      function check_gauss_coefs_4_monitor(gauss)
!
      use m_phys_labels
      use skip_comment_f
!
      type(picked_spectrum_data), intent(in) :: gauss
!
      integer(kind = kint) :: nmode_read
      real(kind = kreal) :: radius_read
!
      character(len=255) :: tmpchara
!
!
      call skip_comment(tmpchara,id_gauss_coef)
      read(id_gauss_coef,*) nmode_read, radius_read
!      write(*,*) 'num_mode', gauss%num_sph_mode, nmode_read
!      write(*,*) 'radius_gauss', gauss%radius_gl(1), radius_read
      if(gauss%num_sph_mode .ne. nmode_read) then
        write(*,*) 'Number of Gauss coefficients does not match ',      &
     &             'with the data in the file'
        check_gauss_coefs_4_monitor = 1
        return
      end if
      if(abs(gauss%radius_gl(1) - radius_read) .gt. 1.0E-8) then
        write(*,*) 'Radius of Gauss coefficients does not match ',      &
     &             'with the data in the file',                         &
     &              gauss%radius_gl(1), radius_read
        check_gauss_coefs_4_monitor = 1
        return
      end if
!
      check_gauss_coefs_4_monitor = 0
      return
!
      end function check_gauss_coefs_4_monitor
!
! -----------------------------------------------------------------------
!
      end module MPI_sph_gauss_coefs_IO

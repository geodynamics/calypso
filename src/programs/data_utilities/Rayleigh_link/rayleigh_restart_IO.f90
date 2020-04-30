!>@file   rayleigh_restart_IO.f90
!!@brief  module rayleigh_restart_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine init_rayleigh_restart_input                          &
!!     &         (i_version, dir, i_step, fld_IO)
!!        type(field_IO), intent(inout) :: fld_IO
!!      subroutine find_rayleigh_restart_address                        &
!!     &          (nri, ltr, kr, l, m, ioffset1, ioffset2)
!!@endverbatim
!
      module rayleigh_restart_IO
!
      use m_precision
      use m_constants
      use t_field_data_IO
      use t_rayleigh_restart_IO
      use set_parallel_file_name
!
      implicit  none
!
      private :: count_rayleigh_restart_field
      private :: set_rayleigh_restart_field
      private :: count_scalar_field_4_raylegh
      private :: count_vector_field_4_raylegh
      private :: add_scalar_field_4_raylegh
      private :: add_vector_field_4_raylegh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_rayleigh_restart_input                            &
     &         (i_version, dir, i_step, fld_IO)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
!
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: ilength
!
!
      if(my_rank .eq. 0) then
        call count_rayleigh_restart_field                               &
     &     (i_version, dir, i_step, fld_IO)
      end if
      call MPI_Bcast(fld_IO%num_field_IO, 1,                            &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call alloc_phys_name_IO(fld_IO)
!
      if(my_rank .eq. 0) then
        call set_rayleigh_restart_field(i_version, dir, i_step, fld_IO)
      end if
      ilength = int(fld_IO%num_field_IO * kchara)
      call MPI_Bcast(fld_IO%fld_name, ilength,                          &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(fld_IO%num_comp_IO, fld_IO%num_field_IO,           &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      end subroutine init_rayleigh_restart_input
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_rayleigh_restart_field                           &
     &         (i_version, dir, i_step, fld_IO)
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      fld_IO%num_field_IO                                               &
     &     = count_vector_field_4_raylegh(i_version, dir, i_step,       &
     &                                    wchar, zchar)
      fld_IO%num_field_IO = fld_IO%num_field_IO                         &
     &     + count_scalar_field_4_raylegh(i_version, dir, i_step,       &
     &                                    pchar)
      fld_IO%num_field_IO = fld_IO%num_field_IO                         &
     &     + count_scalar_field_4_raylegh(i_version, dir, i_step,       &
     &                                    tchar)
      fld_IO%num_field_IO = fld_IO%num_field_IO                         &
      &    + count_vector_field_4_raylegh(i_version, dir, i_step,       &
     &                                    cchar, achar)
!
      fld_IO%num_field_IO = fld_IO%num_field_IO                         &
     &     + count_vector_field_4_raylegh(i_version, dir, i_step,       &
     &                                    wabchar, zabchar)
!      fld_IO%num_field_IO = fld_IO%num_field_IO                        &
!     &     + count_scalar_field_4_raylegh(i_version, dir, i_step,      &
!     &                                    pabchar)
      fld_IO%num_field_IO = fld_IO%num_field_IO                         &
     &     + count_scalar_field_4_raylegh(i_version, dir, i_step,       &
     &                                    tabchar)
      fld_IO%num_field_IO = fld_IO%num_field_IO                         &
     &     + count_vector_field_4_raylegh(i_version, dir, i_step,       &
     &                                    cabchar, aabchar)
!
      end subroutine count_rayleigh_restart_field
!
!-----------------------------------------------------------------------
!
      subroutine set_rayleigh_restart_field                             &
     &         (i_version, dir, i_step, fld_IO)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
!
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: icou
!
!
      icou = 0
      call add_vector_field_4_raylegh(i_version, dir, i_step,           &
     &    wchar, zchar, velocity%name, icou, fld_IO)
      call add_scalar_field_4_raylegh(i_version, dir, i_step,           &
     &    pchar, pressure%name, icou, fld_IO)
      call add_scalar_field_4_raylegh(i_version, dir, i_step,           &
     &    tchar, temperature%name, icou, fld_IO)
      call add_vector_field_4_raylegh(i_version, dir, i_step,           &
     &    cchar, achar, magnetic_field%name, icou, fld_IO)
!
!
      call add_vector_field_4_raylegh(i_version, dir, i_step,           &
     &    wabchar, zabchar, previous_momentum%name, icou, fld_IO)
!      call add_scalar_field_4_raylegh(i_version, dir, i_step,          &
!     &    pabchar, previous_pressure%name, icou, fld_IO)
      call add_scalar_field_4_raylegh(i_version, dir, i_step,           &
     &    tabchar, previous_heat%name, icou, fld_IO)
      call add_vector_field_4_raylegh(i_version, dir, i_step,           &
     &    cabchar, aabchar, previous_induction%name, icou, fld_IO)
!
      end subroutine set_rayleigh_restart_field
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      integer(kind= kint) function count_scalar_field_4_raylegh         &
     &                  (i_version, dir, i_step, sclchar)
!
      use delete_data_files
      use sel_read_rayleigh_restart
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: sclchar
!
      character(len = kchara) :: file1
!
!
      file1 = sel_rayleigh_file_name(i_version, dir, i_step, sclchar)
      if(check_file_exist(file1) .eq. 0) then
        count_scalar_field_4_raylegh = 1
      else
        count_scalar_field_4_raylegh = 0
      end if
!
      end function count_scalar_field_4_raylegh
!
!-----------------------------------------------------------------------
!
      integer(kind= kint) function count_vector_field_4_raylegh         &
     &                  (i_version, dir, i_step, polchar, torchar)
!
      use delete_data_files
      use sel_read_rayleigh_restart
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: polchar
      character(len = kchara), intent(in) :: torchar
!
      character(len = kchara) :: file1, file2
!
!
      file1 = sel_rayleigh_file_name(i_version, dir, i_step, polchar)
      file2 = sel_rayleigh_file_name(i_version, dir, i_step, torchar)
      if((check_file_exist(file1)+check_file_exist(file2)) .eq. 0) then
        count_vector_field_4_raylegh = 1
      else
        count_vector_field_4_raylegh = 0
      end if
!
      end function count_vector_field_4_raylegh
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scalar_field_4_raylegh                             &
     &         (i_version, dir, i_step, sclchar, fhd_fld, icou, fld_IO)
!
      use m_phys_constants
      use delete_data_files
      use sel_read_rayleigh_restart
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: sclchar
      character(len = kchara), intent(in) :: fhd_fld
      integer(kind = kint), intent(inout) :: icou
      type(field_IO), intent(inout) :: fld_IO
!
      character(len = kchara) :: file1
!
!
      file1 = sel_rayleigh_file_name(i_version, dir, i_step, sclchar)
      if(check_file_exist(file1) .eq. 0) then
        icou = icou + 1
        fld_IO%fld_name(icou) =    fhd_fld
        fld_IO%num_comp_IO(icou) = n_scalar
      end if
!
      end subroutine add_scalar_field_4_raylegh
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_field_4_raylegh(i_version, dir, i_step,     &
     &          polchar, torchar, fhd_fld, icou, fld_IO)
!
      use m_phys_constants
      use delete_data_files
      use sel_read_rayleigh_restart
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: polchar
      character(len = kchara), intent(in) :: torchar
      character(len = kchara), intent(in) :: fhd_fld
      integer(kind = kint), intent(inout) :: icou
      type(field_IO), intent(inout) :: fld_IO
!
      character(len = kchara) :: file1, file2
!
!
      file1 = sel_rayleigh_file_name(i_version, dir, i_step, polchar)
      file2 = sel_rayleigh_file_name(i_version, dir, i_step, torchar)
      if((check_file_exist(file1)+check_file_exist(file2)) .eq. 0) then
        icou = icou + 1
        fld_IO%fld_name(icou) =    fhd_fld
        fld_IO%num_comp_IO(icou) = n_solenoid
      end if
!
      end subroutine add_vector_field_4_raylegh
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_rayleigh_restart_address                          &
     &          (nri, ltr, kr, l, m, ioffset1, ioffset2)
!
      integer(kind = kint), intent(in) :: nri, ltr, kr, l, m
      integer(kind = kint_gl), intent(inout) :: ioffset1, ioffset2
!
      integer(kind = kint_gl) :: jmax_h, ioffset
!
      jmax_h = 1 + ltr*(ltr+3) / 2
      ioffset = (l - m + 1) + m * (2*ltr +3 - m) / 2
      ioffset1 = ioffset + (kr - 1) * jmax_h
      ioffset2 = ioffset1 + jmax_h * nri
      ioffset1 = (ioffset1 - 1) * kreal
      ioffset2 = (ioffset2 - 1) * kreal
!
      end subroutine find_rayleigh_restart_address
!
!-----------------------------------------------------------------------
!
      end module rayleigh_restart_IO

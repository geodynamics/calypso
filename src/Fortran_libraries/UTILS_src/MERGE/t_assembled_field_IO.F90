!>@file  t_assembled_field_IO.f90
!!       module t_assembled_field_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief gzipped data IO for 
!!
!!@verbatim
!!      subroutine sel_write_SPH_assemble_field(num_pe, istep_fld,      &
!!     &          nloop, fst_IO_param, t_IO, fld_IO)
!!        type(field_IO_params), intent(in) :: fst_IO_param
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO(nloop)
!!
!!   Data format for the merged ascii field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Each field data  (Itarate 7.1 - 7.3)
!!      7.1   Field name
!!      7.2   List of data size (Byte)
!!      7.3   Field data
!!
!!   Data format for the merged binary field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Field names
!!     8.   List of data size (Byte)
!!     9.   All Field data
!!@endverbatim
!
      module t_assembled_field_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_file_IO_parameter
      use t_time_data
      use t_field_data_IO
      use m_calypso_mpi_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_SPH_assemble_field(num_pe, istep_fld,        &
     &          nloop, fst_IO_param, t_IO, fld_IO)
!
      use field_IO_select
      use set_field_file_names
#ifdef ZLIB_IO
      use gz_assembled_field_MPI_IO
#endif
!
      integer(kind = kint), intent(in) :: istep_fld
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: nloop
      type(field_IO_params), intent(in) :: fst_IO_param
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO(nloop)
!
      integer :: id_rank
      integer(kind = kint) :: iloop
      character(len=kchara) :: file_name
!
!
#ifdef ZLIB_IO
      if(num_pe .ne. nprocs) then
        do iloop = 1, nloop
          id_rank = int(my_rank + (iloop-1) * nprocs)
!
          file_name = set_SPH_fld_file_name                             &
     &            (fst_IO_param%file_prefix, fst_IO_param%iflag_format, &
     &             id_rank, istep_fld)
        end do 
!
        if(fst_IO_param%iflag_format                                    &
     &       .eq. iflag_single+id_gzip_bin_file_fmt) then
          call gz_write_step_asbl_fld_mpi_b                             &
     &         (file_name, num_pe, id_rank, nloop, fld_IO, t_IO)
          return
        else if(fst_IO_param%iflag_format                               &
     &       .eq. iflag_single+id_gzip_txt_file_fmt) then
          call gz_write_step_asbl_fld_mpi                               &
     &         (file_name, num_pe, nloop, fld_IO, t_IO)
          return
        end if
      end if
#endif
!
      do iloop = 1, nloop
        id_rank = int(my_rank + (iloop-1) * nprocs)
!
        call sel_write_step_SPH_field_file(num_pe, id_rank,             &
     &      istep_fld, fst_IO_param, t_IO, fld_IO(iloop))
      end do
!
      end subroutine sel_write_SPH_assemble_field
!
! -----------------------------------------------------------------------
!
      end module t_assembled_field_IO

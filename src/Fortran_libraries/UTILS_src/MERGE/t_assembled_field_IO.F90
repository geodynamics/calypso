!>@file  t_assembled_field_IO.f90
!!       module t_assembled_field_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief gzipped data IO for 
!!
!!@verbatim
!!      subroutine sel_write_SPH_assemble_field                         &
!!     &         (nprocs_in, istep_fld, nloop, fld_IO)
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
      subroutine sel_write_SPH_assemble_field                           &
     &         (nprocs_in, istep_fld, nloop, fld_IO)
!
      use field_IO_select
      use set_field_file_names
!#ifdef ZLIB_IO
!      use gz_assembled_field_MPI_IO
!#endif
!
      integer(kind = kint), intent(in) :: istep_fld
      integer(kind = kint), intent(in) :: nloop, nprocs_in
!
      type(field_IO), intent(inout) :: fld_IO(nloop)
!
      integer(kind = kint) :: iloop, id_rank
      character(len=kchara) :: file_name
!
!
!#ifdef ZLIB_IO
!      if(nprocs_in .ne. nprocs) then
!        do iloop = 1, nloop
!          id_rank = my_rank + (iloop-1) * nprocs
!
!          call set_SPH_fld_file_name(fld_IO(iloop)%file_prefix,        &
!     &       fld_IO(iloop)%iflag_file_fmt, id_rank, istep_fld,         &
!     &       file_name)
!        end do 
!
!        if(fld_IO(1)%iflag_file_fmt                                    &
!     &       .eq. iflag_single+id_gzip_bin_file_fmt) then
!          call gz_write_step_asbl_fld_mpi_b                            &
!     &         (file_name, nprocs_in, nloop, fld_IO)
!          return
!        else if(fld_IO(1)%iflag_file_fmt                               &
!     &       .eq. iflag_single+id_gzip_txt_file_fmt) then
!          call gz_write_step_asbl_fld_mpi                              &
!     &         (file_name, nprocs_in, nloop, fld_IO)
!          return
!        end if
!      end if
!#endif
!
      do iloop = 1, nloop
        id_rank = my_rank + (iloop-1) * nprocs
!
        call sel_write_step_SPH_field_file                              &
     &     (nprocs_in, id_rank, istep_fld, fld_IO(iloop))
      end do
!
      end subroutine sel_write_SPH_assemble_field
!
! -----------------------------------------------------------------------
!
      end module t_assembled_field_IO

!>@file   delete_field_smp.f90
!!        module delete_field_smp
!!
!!@author H. Matsui
!!@date Programmed in May., 2009
!!
!>@brief Clear field to zero or constants
!!
!!@verbatim
!!      subroutine delete_phys_data_smp(numnod, ist, ied,               &
!!     &          ntot_comp, numdir, i_field, field)
!!      subroutine constant_phys_data_smp(const, numnod, ist, ied,      &
!!     &          ntot_comp, numdir, i_field, field)
!!        real(kind = kreal), intent(in) :: const
!!        integer(kind = kint), intent(in) :: numnod, ist, ied
!!        integer(kind = kint), intent(in) :: ntot_comp, numdir, i_field
!!        real(kind = kreal), intent(inout) :: field(numnod,ntot_comp)
!!
!!      subroutine constant_scalar_smp(const, numnod, ist, ied, scalar)
!!        real(kind = kreal), intent(inout) :: scalar(numnod)
!!      subroutine constant_vector_smp(const, numnod, ist, ied, vector)
!!        real(kind = kreal), intent(inout) :: vector(numnod,3)
!!      subroutine constant_sym_tensor_smp(const, numnod,               &
!!     &                                   ist, ied, tensor)
!!      subroutine constant_arb_vect_smp(const, numnod,                 &
!!     &                                 ist, ied, numdir, vector)
!!        real(kind = kreal), intent(in) :: const
!!        integer(kind = kint), intent(in) :: numnod, ist, ied, numdir
!!        real(kind = kreal), intent(inout) :: tensor(numnod,6)
!!        real(kind = kreal), intent(inout) :: vector(numnod,numdir)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!
!!@n @param  ntot_comp total number of components of fields
!!@n @param  field(numnod,ntot_comp)  fields data
!!@n @param  numdir    number of components of field to be cleared
!!@n @param  i_field   address of field to be cleared
!
      module delete_field_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine delete_phys_data_smp(numnod, ist, ied,                 &
     &          ntot_comp, numdir, i_field, field)
!
      integer (kind=kint), intent(in) :: numnod, ist, ied
      integer (kind=kint), intent(in) :: ntot_comp, numdir, i_field
!
      real (kind=kreal), intent(inout) :: field(numnod,ntot_comp)
!
!
      call constant_phys_data_smp(zero, numnod, ist, ied,               &
     &                            ntot_comp, numdir, i_field, field)
!
      end subroutine delete_phys_data_smp
!
! ----------------------------------------------------------------------
!
      subroutine constant_phys_data_smp(const, numnod, ist, ied,        &
     &          ntot_comp, numdir, i_field, field)
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: numnod, ist, ied
      integer(kind = kint), intent(in) :: ntot_comp, numdir, i_field
!
      real(kind = kreal), intent(inout) :: field(numnod,ntot_comp)
!
!
      if(numdir .eq. ione) then
        call constant_scalar_smp(const, numnod, ist, ied,               &
     &                           field(1,i_field))
      else if(numdir .eq. ithree) then
        call constant_vector_smp(const, numnod, ist, ied,               &
     &                           field(1,i_field))
      else if(numdir .eq. isix) then
        call constant_sym_tensor_smp(const, numnod, ist, ied,           &
     &                               field(1,i_field))
      else
        call constant_arb_vect_smp(const, numnod, ist, ied,             &
     &                             numdir, field(1,i_field))
      end if
!
      end subroutine constant_phys_data_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine constant_scalar_smp(const, numnod, ist, ied, scalar)
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: ist, ied, numnod
      real(kind = kreal), intent(inout) :: scalar(numnod)
!
!$omp workshare
          scalar(ist:ied) = const
!$omp end workshare nowait
!
      end subroutine constant_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine constant_vector_smp(const, numnod, ist, ied, vector)
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: ist, ied, numnod
      real(kind = kreal), intent(inout) :: vector(numnod,3)
!
!
!$omp workshare
      vector(ist:ied,1) = const
      vector(ist:ied,2) = const
      vector(ist:ied,3) = const
!$omp end workshare nowait
!
      end subroutine constant_vector_smp
!
! ----------------------------------------------------------------------
!
      subroutine constant_sym_tensor_smp(const, numnod,                 &
     &                                   ist, ied, tensor)
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: ist, ied, numnod
      real(kind = kreal), intent(inout) :: tensor(numnod,6)
!
!
!$omp workshare
      tensor(ist:ied,1) = const
      tensor(ist:ied,2) = const
      tensor(ist:ied,3) = const
      tensor(ist:ied,4) = const
      tensor(ist:ied,5) = const
      tensor(ist:ied,6) = const
!$omp end workshare nowait
!
      end subroutine constant_sym_tensor_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine constant_arb_vect_smp(const, numnod,                   &
     &                                 ist, ied, numdir, vector)
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: numnod, ist, ied, numdir
      real(kind = kreal), intent(inout) :: vector(numnod,numdir)
!
      integer (kind=kint) :: nd
!
!
      do nd = 1, numdir
!$omp workshare
          vector(ist:ied,nd) = const
!$omp end workshare nowait
      end do
!
      end subroutine constant_arb_vect_smp
!
! ----------------------------------------------------------------------
!
      end module delete_field_smp

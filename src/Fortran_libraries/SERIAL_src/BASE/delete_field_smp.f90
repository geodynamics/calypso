!>@file   delete_field_smp.f90
!!@brief  module delete_field_smp
!!
!!@author H. Matsui
!!@date Programmed in May., 2009
!
!>@brief Clear field to zero
!!
!!@verbatim
!!      subroutine delete_phys_data_smp(numnod, ist, ied,               &
!!     &          ntot_comp, numdir, i_field, field)
!!
!!      subroutine delete_scalar_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          scalar)
!!      subroutine delete_vector_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          vector)
!!      subroutine delete_sym_tensor_smp(np_smp, numnod, inod_smp_stack,&
!!     &          tensor)
!!      subroutine delete_arb_vect_smp(np_smp, numnod, inod_smp_stack,  &
!!     &          numdir, vector)
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
!!
!!@n @param  scalar(numnod)   scalar field to be cleared
!!@n @param  vector(numnod,3) vector field to be cleared
!!@n @param  tensor(numnod,6) symmetric tensor field to be cleared
      module delete_field_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: delete_scalar_smp, delete_vector_smp
      private :: delete_sym_tensor_smp, delete_arb_vect_smp
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
      if(numdir .eq. ione) then
        call delete_scalar_smp(numnod, ist, ied, field(1,i_field) )
      else if(numdir .eq. ithree) then
        call delete_vector_smp(numnod, ist, ied, field(1,i_field) )
      else if(numdir .eq. isix) then
        call delete_sym_tensor_smp(numnod, ist, ied, field(1,i_field) )
      else
        call delete_arb_vect_smp(numnod, ist, ied,                      &
     &      numdir, field(1,i_field) )
      end if
!
      end subroutine delete_phys_data_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine delete_scalar_smp(numnod, ist, ied, scalar)
!
      integer (kind=kint), intent(in) :: ist, ied, numnod
      real (kind=kreal), intent(inout) :: scalar(numnod)
!
!$omp workshare
          scalar(ist:ied) = zero
!$omp end workshare nowait
!
      end subroutine delete_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine delete_vector_smp(numnod, ist, ied, vector)
!
      integer (kind=kint), intent(in) :: ist, ied, numnod
      real (kind=kreal), intent(inout) :: vector(numnod,3)
!
!
!$omp workshare
      vector(ist:ied,1) = zero
      vector(ist:ied,2) = zero
      vector(ist:ied,3) = zero
!$omp end workshare nowait
!
      end subroutine delete_vector_smp
!
! ----------------------------------------------------------------------
!
      subroutine delete_sym_tensor_smp(numnod, ist, ied, tensor)
!
      integer (kind=kint), intent(in) :: ist, ied, numnod
      real (kind=kreal), intent(inout) :: tensor(numnod,6)
!
!
!$omp workshare
      tensor(ist:ied,1) = zero
      tensor(ist:ied,2) = zero
      tensor(ist:ied,3) = zero
      tensor(ist:ied,4) = zero
      tensor(ist:ied,5) = zero
      tensor(ist:ied,6) = zero
!$omp end workshare nowait
!
      end subroutine delete_sym_tensor_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine delete_arb_vect_smp(numnod, ist, ied, numdir, vector)
!
      integer (kind=kint), intent(in) :: numnod, ist, ied, numdir
      real (kind=kreal), intent(inout) :: vector(numnod,numdir)
!
      integer (kind=kint) :: nd
!
!
      do nd = 1, numdir
!$omp workshare
          vector(ist:ied,nd) = zero
!$omp end workshare nowait
      end do
!
      end subroutine delete_arb_vect_smp
!
! ----------------------------------------------------------------------
!
      end module delete_field_smp

!>@file   delete_field_smp.f90
!!@brief  module delete_field_smp
!!
!!@author H. Matsui
!!@date Programmed in May., 2009
!
!>@brief Clear field to zero
!!
!!@verbatim
!!      subroutine delete_phys_data_smp(np_smp, numnod, inod_smp_stack, &
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
!
      module delete_field_smp
!
      use m_precision
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
      subroutine delete_phys_data_smp(np_smp, numnod, inod_smp_stack,   &
     &          ntot_comp, numdir, i_field, field)
!
      use m_constants
!
      integer (kind=kint), intent(in) :: np_smp, numnod
      integer (kind=kint), intent(in) :: ntot_comp, numdir, i_field
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real (kind=kreal), intent(inout) :: field(numnod,ntot_comp)
!
!
      if(numdir .eq. ione) then
        call delete_scalar_smp(np_smp, numnod, inod_smp_stack,          &
     &      field(1,i_field) )
      else if(numdir .eq. ithree) then
        call delete_vector_smp(np_smp, numnod, inod_smp_stack,          &
     &      field(1,i_field) )
      else if(numdir .eq. isix) then
        call delete_sym_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &      field(1,i_field) )
      else
        call delete_arb_vect_smp(np_smp, numnod, inod_smp_stack,        &
     &      numdir, field(1,i_field) )
      end if
!
      end subroutine delete_phys_data_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine delete_scalar_smp(np_smp, numnod, inod_smp_stack,      &
     &          scalar)
!
      use m_constants
!
      integer (kind=kint), intent(in) :: np_smp, numnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real (kind=kreal), intent(inout) :: scalar(numnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          scalar(inod) = zero
        end do
      end do
!$omp end do nowait
!
      end subroutine delete_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine delete_vector_smp(np_smp, numnod, inod_smp_stack,      &
     &          vector)
!
      use m_constants
!
      integer (kind=kint), intent(in) :: np_smp, numnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real (kind=kreal), intent(inout) :: vector(numnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          vector(inod,1) = zero
          vector(inod,2) = zero
          vector(inod,3) = zero
        end do
      end do
!$omp end do nowait
!
      end subroutine delete_vector_smp
!
! ----------------------------------------------------------------------
!
      subroutine delete_sym_tensor_smp(np_smp, numnod, inod_smp_stack,  &
     &          tensor)
!
      use m_constants
!
      integer (kind=kint), intent(in) :: np_smp, numnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real (kind=kreal), intent(inout) :: tensor(numnod,6)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          tensor(inod,1) = zero
          tensor(inod,2) = zero
          tensor(inod,3) = zero
          tensor(inod,4) = zero
          tensor(inod,5) = zero
          tensor(inod,6) = zero
        end do
      end do
!$omp end do nowait
!
      end subroutine delete_sym_tensor_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine delete_arb_vect_smp(np_smp, numnod, inod_smp_stack,    &
     &          numdir, vector)
!
      use m_constants
!
      integer (kind=kint), intent(in) :: np_smp, numnod, numdir
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real (kind=kreal), intent(inout) :: vector(numnod,numdir)
!
      integer (kind=kint) :: iproc, inod, ist, ied, nd
!
!
!$omp do private(inod,ist,ied,nd)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
        do nd = 1, numdir
!cdir nodep
          do inod = ist, ied
            vector(inod,nd) = zero
          end do
        end do
      end do
!$omp end do nowait
!
      end subroutine delete_arb_vect_smp
!
! ----------------------------------------------------------------------
!
      end module delete_field_smp

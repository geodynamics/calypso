!Author: Harsha Lokavarapu
!Date: 9/13/14

      module cuda_optimizations 
        use calypso_mpi
        use m_work_time
        use iso_c_binding
        use m_precision
        use m_spheric_parameter
        use m_spheric_param_smp
        use m_machine_parameter
        use m_schmidt_poly_on_rtm
        use schmidt_fix_m
        use m_work_4_sph_trans
        use FFT_selector
        use merge_polidal_toroidal_v
        use spherical_SRs_N
        use const_coriolis_sph_rlm
        use legendre_bwd_trans_org

        implicit none
  
        contains
      
        subroutine alloc_mem_4_gpu(ncomp)
        integer(kind = kint), intent(in) :: ncomp
          call initgpu(nnod_rtp, nnod_rtm, nnod_rlm, nidx_rtm(1),       &
     &                      nidx_rlm(1), istep_rtm(1), istep_rlm(1),    &
     &                      ncomp, l_truncation)
#ifdef CUDA_DEBUG
          call setptrs(idx_gl_1d_rlm_j(1,1))
#endif
        end subroutine alloc_mem_4_gpu

        subroutine set_mem_4_gpu
          call memcpy_h2d(lstack_rlm(0), a_r_1d_rlm_r(1),g_colat_rtm(1),&
     &                         g_sph_rlm(1,3))
        end subroutine set_mem_4_gpu

        subroutine legendre_b_trans_vector_cuda                         &
     &         (ncomp, nvector, nscalar, sp_rlm, vr_rtm)
!
        integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
        real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
        real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
        
#ifdef CUDA_DEBUG
        integer(kind = kint) :: l, m, j, l_rtm, k_rtm, nd, ip_rtm 
        integer(kind = kint) :: jst, jed, mp_rlm, j_rlm
        character(len=5) :: c_rank
        character(len=35) :: fileName        
        write(c_rank, '(I1)') my_rank
#endif 

!        integer(kind = kint) :: ip, kst, ked, jst, jed

!        kst = idx_rtm_smp_stack(ip-1,1) + 1 
!        ked = idx_rtm_smp_stack(np_smp,1) 
!        jst = lstack_rlm(0) + 1
!        jed = lstack_rlm(nidx_rtm(3))

!        call transform_b(ncomp, nvector, nscalar, kst, ked, jst, jed) 
        call transform_b(ncomp, nvector, nscalar, vr_rtm(1)) 
! End of transform, the physical data is copied into vr_rtm

#ifdef CUDA_DEBUG
       call legendre_b_trans_vector_org(ncomp, nvector, sp_rlm, vr_rtm)

       fileName = "bwd_SHT_domainId_"//c_rank//"_base.dat"         
       open (unit=1, file=fileName, action="write", status="replace")
       write(1, '(a)') 'shell,theta, vector,order, degree,      &
     &      g_sph_rlm, P_smdt, dP_smdt, vr_rtm[0], vr_rtm[1], vr_rtm[2]'

       do k_rtm = 1, nidx_rtm(1) 
! l_trncation + 1 is s.t. m==0 and +2 is s.t. m==1
         do mp_rlm = l_truncation+1, l_truncation+2
           jst = lstack_rlm(mp_rlm-1) + 1
           jed = lstack_rlm(mp_rlm)
           do l_rtm = 1, nidx_rtm(2) 
             do nd = 1, nvector
               ip_rtm = 3*nd + ncomp*(l_rtm-1)*istep_rtm(2) +         &
     &            (k_rtm-1)*istep_rtm(1) + (mp_rlm-1)*istep_rtm(3)
                 do j_rlm = jst, jed
                   m = idx_gl_1d_rlm_j(j_rlm,3)
                   l = idx_gl_1d_rlm_j(j_rlm,2)
                   if (m .EQ. 0) then
                   write(1,*) k_rtm, g_colat_rtm(l_rtm), nd, m, l,  &
     &               g_sph_rlm(j_rlm,3),             &
     &                     P_jl(j_rlm,l_rtm),            &
     &                   dPdt_jl(j_rlm,l_rtm)                         &
     &               , vr_rtm(ip_rtm-2), vr_rtm(ip_rtm-1),vr_rtm(ip_rtm)
                   else if (m .EQ. 1) then
                   write(1,*) k_rtm, g_colat_rtm(l_rtm), nd, m, l,  &
     &               g_sph_rlm(j_rlm,3),             &
     &                     P_jl(j_rlm,l_rtm)
                   end if
                 end do
               end do
             end do
           end do
         end do
      close(1)
      stop
#endif

        end subroutine legendre_b_trans_vector_cuda                     

        subroutine fwd_transform(ncomp_, nvector_, nscalar_)
          integer(kind=kint), intent(in) :: ncomp_, nvector_, nscalar_
        !  call transform_f(ncomp_, nvector_, nscalar_)
        end subroutine fwd_transform

        subroutine cpy_spectrum_dat_2_gpu(ncomp, sp_rlm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
          
          call set_spectrum_data(sp_rlm(1))
        end subroutine cpy_spectrum_dat_2_gpu

       
        subroutine cpy_physical_dat_from_gpu(ncomp, vr_rtm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
          
          call retrieve_physical_data(vr_rtm(1))
        end subroutine cpy_physical_dat_from_gpu

        subroutine finalize_gpu
          call cleangpu
        end subroutine finalize_gpu 
 
      end module cuda_optimizations 

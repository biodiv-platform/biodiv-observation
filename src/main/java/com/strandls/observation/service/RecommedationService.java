/**
 * 
 */
package com.strandls.observation.service;

import com.strandls.observation.pojo.RecoIbp;

/**
 * @author Abhishek Rudra
 *
 */
public interface RecommedationService {
	
	public RecoIbp fetchRecoVote(Long id);
}

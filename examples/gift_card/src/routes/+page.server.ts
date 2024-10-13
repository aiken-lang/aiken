import { readValidators } from '$lib/utils';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async () => {
  const validator = readValidators().giftCard;

  return { validator };
};
